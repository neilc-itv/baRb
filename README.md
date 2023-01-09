
# baRb

<!-- badges: start -->
<!-- badges: end -->

Functions for working with BARB's TV spot API

### Installation

```
remotes::install::github("neilc-itv/baRb")
```

### Getting started

BaRb requires a username and password to be set using environment variables.
Use these commands or set in your .Renviron file.

```
Sys.setenv(BARB_API_USERNAME = "username")
Sys.setenv(BARB_API_PASSWORD = "password")
```


Get a list of available advertisers with:

```
barb_get_advertisers("2022-01-01", "2022-01-31")
```

And a spot list for a specific advertiser with:

```
barb_get_spots(
  min_transmission_date = "2022-01-01",
  max_transmission_date = "2022-12-31",
  advertiser_name = "PLAYMOBIL UK")
```
