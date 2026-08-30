# Unsere Wege - preprocessing
#
# Input:
#   Data/1_Input/*.gpx
#
# Output:
#   Data/2_Intermediate/
#     <day>.gpx
#     <day>.shp + sidecars
#     UnsereWege_manifest.csv
#
# Cleaning is done here only. The dashboard never reads Data/1_Input.
#
# Packages:
# install.packages(c("xml2", "sf"))

required_packages <- c(
  "xml2",
  "sf"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install required package(s): ",
    paste(
      missing_packages,
      collapse = ", "
    )
  )
}


get_script_path <- function() {
  args <- commandArgs(
    trailingOnly = FALSE
  )

  file_arg <- grep(
    "^--file=",
    args,
    value = TRUE
  )

  if (length(file_arg) > 0) {
    return(
      normalizePath(
        sub(
          "^--file=",
          "",
          file_arg[1]
        )
      )
    )
  }

  ofile <- tryCatch(
    sys.frame(1)$ofile,
    error = function(e) NULL
  )

  if (
    !is.null(ofile) &&
    nzchar(ofile)
  ) {
    return(
      normalizePath(ofile)
    )
  }

  if (
    requireNamespace(
      "rstudioapi",
      quietly = TRUE
    ) &&
    rstudioapi::isAvailable()
  ) {
    p <- rstudioapi::getSourceEditorContext()$path

    if (nzchar(p)) {
      return(
        normalizePath(p)
      )
    }
  }

  stop(
    paste0(
      "Could not determine the script location. ",
      "Save this file in Scripts/ and run it with Source."
    )
  )
}


script_path <- get_script_path()
script_dir  <- dirname(script_path)

project_dir <- if (
  basename(script_dir) == "Scripts"
) {
  dirname(script_dir)
} else {
  stop(
    "01_Preprocess.R must be stored in the project's Scripts/ folder."
  )
}

input_dir <- file.path(
  project_dir,
  "Data",
  "1_Input"
)

intermediate_dir <- file.path(
  project_dir,
  "Data",
  "2_Intermediate"
)

if (!dir.exists(input_dir)) {
  stop(
    "Input directory not found: ",
    input_dir
  )
}

dir.create(
  intermediate_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

tz_local <- "Europe/Vienna"


# ============================================================
# 1. READ RAW GPX
# ============================================================

files <- list.files(
  input_dir,
  pattern = "\\.gpx$",
  full.names = TRUE,
  ignore.case = TRUE
)

files <- files[
  grepl(
    "^(Track_)?(FL|GAL|JR|AB|ECRINS)[0-9]+\\.gpx$",
    basename(files),
    ignore.case = TRUE
  )
]

if (length(files) == 0) {
  stop(
    "No recognized GPX files found in: ",
    input_dir
  )
}


read_gpx_raw <- function(file) {
  doc <- xml2::read_xml(file)

  p <- xml2::xml_find_all(
    doc,
    ".//*[local-name()='trkpt']"
  )

  if (length(p) == 0) {
    stop(
      "No trackpoints found in: ",
      basename(file)
    )
  }

  time_txt <- xml2::xml_text(
    xml2::xml_find_first(
      p,
      "./*[local-name()='time']"
    )
  )

  ele_txt <- xml2::xml_text(
    xml2::xml_find_first(
      p,
      "./*[local-name()='ele']"
    )
  )

  time_utc <- as.POSIXct(
    time_txt,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )

  elevation <- as.numeric(
    ele_txt
  )

  if (
    anyNA(time_utc) ||
    anyNA(elevation)
  ) {
    stop(
      "Missing/invalid timestamp or elevation in: ",
      basename(file)
    )
  }

  source_track <- sub(
    "\\.gpx$",
    "",
    basename(file),
    ignore.case = TRUE
  )

  source_track <- sub(
    "^Track_",
    "",
    source_track,
    ignore.case = TRUE
  )

  group <- toupper(
    sub(
      "[0-9]+$",
      "",
      source_track
    )
  )

  data.frame(
    source_file  = basename(file),
    source_track = toupper(source_track),
    group        = group,
    time_utc     = time_utc,
    lon          = as.numeric(
      xml2::xml_attr(
        p,
        "lon"
      )
    ),
    lat          = as.numeric(
      xml2::xml_attr(
        p,
        "lat"
      )
    ),
    elevation    = elevation,
    stringsAsFactors = FALSE
  )
}


raw_points <- do.call(
  rbind,
  lapply(
    files,
    read_gpx_raw
  )
)

expected_groups <- c(
  "AB",
  "JR",
  "GAL",
  "ECRINS",
  "FL"
)

missing_groups <- setdiff(
  expected_groups,
  unique(raw_points$group)
)

if (length(missing_groups) > 0) {
  stop(
    "Missing raw GPX group(s): ",
    paste(
      missing_groups,
      collapse = ", "
    )
  )
}

raw_points <- raw_points[
  order(
    raw_points$group,
    raw_points$time_utc,
    raw_points$source_track
  ),
]

rownames(raw_points) <- NULL


# ============================================================
# 2. SOURCE-SPECIFIC CLEANING
# ============================================================

# ECRINS1 is duplicated exactly at the start of ECRINS2.
# Keep the first occurrence only.
ecrins_idx <- which(
  raw_points$group == "ECRINS"
)

n_ecrins_duplicate <- 0

if (length(ecrins_idx) > 0) {
  ecrins_duplicate <- duplicated(
    raw_points[
      ecrins_idx,
      c(
        "time_utc",
        "lon",
        "lat",
        "elevation"
      )
    ]
  )

  n_ecrins_duplicate <- sum(
    ecrins_duplicate
  )

  if (n_ecrins_duplicate > 0) {
    raw_points <- raw_points[
      -ecrins_idx[ecrins_duplicate],
    ]
  }
}

rownames(raw_points) <- NULL


# ECRINS1:
# the walk returns to the start/parking area at 13:33 UTC.
# After a short stationary period the recording continues by car.
# Keep the walking route through its last genuine walking point.
ecrins1_end_utc <- as.POSIXct(
  "2025-09-19 13:33:15",
  tz = "UTC"
)

drop_ecrins1_car <- (
  raw_points$group == "ECRINS" &
    raw_points$source_track == "ECRINS1" &
    raw_points$time_utc > ecrins1_end_utc
)

n_ecrins1_car <- sum(
  drop_ecrins1_car
)

raw_points <- raw_points[
  !drop_ecrins1_car,
]


# ECRINS2:
# remove camping drift and the relocation to the next trailhead.
# The first genuine point of the next walk is retained.
ecrins2_start_utc <- as.POSIXct(
  "2025-09-20 13:22:23",
  tz = "UTC"
)

drop_ecrins2_start <- (
  raw_points$group == "ECRINS" &
    raw_points$source_track == "ECRINS2" &
    raw_points$time_utc < ecrins2_start_utc
)

n_ecrins2_start <- sum(
  drop_ecrins2_start
)

raw_points <- raw_points[
  !drop_ecrins2_start,
]

rownames(raw_points) <- NULL


# ============================================================
# 3. CANONICAL LOCAL CALENDAR DAYS
#
# All source pieces belonging to the same local calendar date are
# merged first. Mini-days are intentionally still present here.
# ============================================================

raw_points$date_local <- as.Date(
  format(
    raw_points$time_utc,
    tz = tz_local,
    format = "%Y-%m-%d"
  )
)

day_lookup <- unique(
  raw_points[
    ,
    c(
      "group",
      "date_local"
    )
  ]
)

day_lookup <- day_lookup[
  order(
    day_lookup$group,
    day_lookup$date_local
  ),
]

day_lookup$raw_day_no <- ave(
  as.numeric(
    day_lookup$date_local
  ),
  day_lookup$group,
  FUN = seq_along
)

day_lookup$day_prefix <- ifelse(
  day_lookup$group == "JR",
  "GR20-",
  ifelse(
    day_lookup$group == "AB",
    "AB",
    day_lookup$group
  )
)

day_lookup$canonical_day_id <- paste0(
  day_lookup$day_prefix,
  day_lookup$raw_day_no
)

raw_key <- paste(
  raw_points$group,
  raw_points$date_local
)

day_key <- paste(
  day_lookup$group,
  day_lookup$date_local
)

ii <- match(
  raw_key,
  day_key
)

raw_points$canonical_day_id <- day_lookup$canonical_day_id[ii]


# ============================================================
# 4. FINAL DAY CLEANING
#
# Exactly as discussed previously:
# these tiny calendar fragments have passed through the full
# source/date preprocessing above and are removed only now.
# ============================================================

exclude_days <- c(
  "GAL1",
  "ECRINS3",
  "ECRINS4"
)

clean_points <- raw_points[
  !raw_points$canonical_day_id %in% exclude_days,
]

clean_days <- day_lookup[
  !day_lookup$canonical_day_id %in% exclude_days,
]

rownames(clean_points) <- NULL
rownames(clean_days)   <- NULL


# Final filenames/day IDs.
#
# Keep the established Galtür IDs GAL2-GAL5.
# Écrins is deliberately renumbered after its two tiny fragments
# are removed, so the visible/clean outputs are ECRINS1-ECRINS3.
clean_days$day_id <- clean_days$canonical_day_id

jj_ecrins <- which(
  clean_days$group == "ECRINS"
)

if (length(jj_ecrins) > 0) {
  clean_days$day_id[jj_ecrins] <- paste0(
    "ECRINS",
    seq_along(jj_ecrins)
  )
}

clean_days$day_no <- ave(
  as.numeric(
    clean_days$date_local
  ),
  clean_days$group,
  FUN = seq_along
)

clean_days$group_label <- ifelse(
  clean_days$group == "AB",
  "Abtenau",
  ifelse(
    clean_days$group == "JR",
    "GR20 Nord",
    ifelse(
      clean_days$group == "GAL",
      "Galtür",
      ifelse(
        clean_days$group == "ECRINS",
        "Écrins",
        ifelse(
          clean_days$group == "FL",
          "Flirsch",
          clean_days$group
        )
      )
    )
  )
)

jj <- match(
  clean_points$canonical_day_id,
  clean_days$canonical_day_id
)

clean_points$day_id      <- clean_days$day_id[jj]
clean_points$day_no      <- clean_days$day_no[jj]
clean_points$group_label <- clean_days$group_label[jj]

clean_points <- clean_points[
  order(
    clean_points$group,
    clean_points$day_no,
    clean_points$time_utc,
    clean_points$source_track
  ),
]

rownames(clean_points) <- NULL


# ============================================================
# 5. OUTPUT HELPERS
# ============================================================

write_clean_gpx <- function(d, file, track_name) {
  time_txt <- format(
    d$time_utc,
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%OS3Z"
  )

  point_lines <- unlist(
    lapply(
      seq_len(nrow(d)),
      function(i) {
        c(
          sprintf(
            '      <trkpt lat="%.8f" lon="%.8f">',
            d$lat[i],
            d$lon[i]
          ),
          sprintf(
            "        <ele>%.2f</ele>",
            d$elevation[i]
          ),
          paste0(
            "        <time>",
            time_txt[i],
            "</time>"
          ),
          "      </trkpt>"
        )
      }
    ),
    use.names = FALSE
  )

  lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    paste0(
      '<gpx version="1.1" creator="Unsere Wege preprocessing" ',
      'xmlns="http://www.topografix.com/GPX/1/1" ',
      'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ',
      'xsi:schemaLocation="http://www.topografix.com/GPX/1/1 ',
      'http://www.topografix.com/GPX/1/1/gpx.xsd">'
    ),
    "  <trk>",
    paste0(
      "    <name>",
      track_name,
      "</name>"
    ),
    "    <trkseg>",
    point_lines,
    "    </trkseg>",
    "  </trk>",
    "</gpx>"
  )

  writeLines(
    lines,
    con = file,
    useBytes = TRUE
  )
}


make_vertex_table <- function(d) {
  local_date <- as.Date(
    format(
      d$time_utc,
      tz = tz_local,
      format = "%Y-%m-%d"
    )
  )

  data.frame(
    X = d$lon,
    Y = d$lat,
    Z = d$elevation,
    date = local_date,
    year = as.integer(
      format(
        d$time_utc,
        tz = tz_local,
        format = "%Y"
      )
    ),
    month = as.integer(
      format(
        d$time_utc,
        tz = tz_local,
        format = "%m"
      )
    ),
    julian = as.integer(
      format(
        d$time_utc,
        tz = tz_local,
        format = "%j"
      )
    ),
    timestamp_utc = format(
      d$time_utc,
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%OS3Z"
    ),
    hour = as.integer(
      format(
        d$time_utc,
        tz = tz_local,
        format = "%H"
      )
    ),
    minute = as.integer(
      format(
        d$time_utc,
        tz = tz_local,
        format = "%M"
      )
    ),
    second = as.numeric(
      format(
        d$time_utc,
        tz = tz_local,
        format = "%OS"
      )
    ),
    stringsAsFactors = FALSE
  )
}


write_clean_shapefile <- function(d, file) {
  v <- make_vertex_table(d)

  # ESRI Shapefile/DBF field names are limited to 10 characters.
  # Therefore the requested `timestamp_utc` field is stored in the
  # shapefile as `time_utc`. Its values remain UTC ISO timestamps.
  names(v)[
    names(v) == "timestamp_utc"
  ] <- "time_utc"

  # Store geometry as ordinary 2D WGS84 POINT geometry.
  # Elevation remains available in the numeric Z attribute column.
  x <- sf::st_as_sf(
    v,
    coords = c(
      "X",
      "Y"
    ),
    crs = 4326,
    remove = FALSE
  )

  sf::st_write(
    x,
    dsn = file,
    driver = "ESRI Shapefile",
    delete_layer = TRUE,
    quiet = TRUE,
    layer_options = "ENCODING=UTF-8"
  )
}


# ============================================================
# 6. WRITE CLEANED DAYS
# ============================================================

day_split <- split(
  clean_points,
  clean_points$day_id
)

manifest <- clean_days[
  ,
  c(
    "day_id",
    "day_no",
    "group",
    "group_label",
    "date_local",
    "canonical_day_id"
  )
]

manifest$gpx_file <- paste0(
  manifest$day_id,
  ".gpx"
)

manifest$shp_file <- paste0(
  manifest$day_id,
  ".shp"
)

manifest$n_points <- NA_integer_

for (i in seq_len(nrow(manifest))) {
  day <- manifest$day_id[i]

  d <- day_split[[day]]

  if (
    is.null(d) ||
    nrow(d) < 2
  ) {
    stop(
      "Cleaned day has fewer than 2 points: ",
      day
    )
  }

  d <- d[
    order(
      d$time_utc,
      d$source_track
    ),
  ]

  gpx_file <- file.path(
    intermediate_dir,
    manifest$gpx_file[i]
  )

  shp_file <- file.path(
    intermediate_dir,
    manifest$shp_file[i]
  )

  write_clean_gpx(
    d,
    gpx_file,
    day
  )

  write_clean_shapefile(
    d,
    shp_file
  )

  manifest$n_points[i] <- nrow(d)
}


manifest <- manifest[
  order(
    manifest$date_local,
    manifest$group,
    manifest$day_no
  ),
]

manifest_file <- file.path(
  intermediate_dir,
  "UnsereWege_manifest.csv"
)

utils::write.table(
  manifest,
  file = manifest_file,
  sep = ";",
  dec = ".",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  fileEncoding = "UTF-8"
)


# ============================================================
# 7. FINAL CHECKS + REPORT
# ============================================================

stopifnot(
  nrow(manifest) == 24,
  !anyDuplicated(
    manifest$day_id
  ),
  all(
    file.exists(
      file.path(
        intermediate_dir,
        manifest$gpx_file
      )
    )
  ),
  all(
    file.exists(
      file.path(
        intermediate_dir,
        manifest$shp_file
      )
    )
  ),
  identical(
    manifest$day_id[
      manifest$group == "ECRINS"
    ],
    c(
      "ECRINS1",
      "ECRINS2",
      "ECRINS3"
    )
  ),
  !"GAL1" %in% manifest$day_id
)

message(
  "Preprocessing complete."
)

message(
  "  Cleaned days written: ",
  nrow(manifest)
)

message(
  "  Exact duplicate Écrins vertices removed: ",
  n_ecrins_duplicate
)

message(
  "  ECRINS1 post-walk/car vertices removed: ",
  n_ecrins1_car
)

message(
  "  ECRINS2 camping/relocation vertices removed: ",
  n_ecrins2_start
)

message(
  "  Intermediate directory: ",
  intermediate_dir
)
