# Running RetailPulse locally with Docker

Docker lets you run RetailPulse on your own PC without installing R or any R
packages. Everything runs inside a container, and you open the dashboard in
your browser.

## 1. Prerequisites

- **Docker Desktop** (Windows or macOS) or **Docker Engine** (Linux).
  Download it from <https://www.docker.com/products/docker-desktop/>.
  - On Windows, enable the WSL 2 backend when the installer asks.
  - Start Docker Desktop and wait until it shows "Engine running".
- **Git**, to clone the repository. Alternatively, download the ZIP from
  GitHub.
- About **2 GB of free disk space** for the image, and an internet connection
  for the first build.

Check that Docker works:

```bash
docker --version
docker run --rm hello-world
```

## 2. Get the project

```bash
git clone https://github.com/mohitahujaa/Retail-Insights.git
cd Retail-Insights
```

The dataset (`data/raw/online_retail.xlsx`) is included in the repository, so
you don't need to download anything else.

## 3. Build the image (first time only, about 3–6 minutes)

```bash
docker build -t retailpulse .
```

What the build does:
1. Starts from `rocker/r-ver:4.4.1` (R 4.4.1 with CRAN packages pinned to a
   fixed date, so everyone gets the same versions).
2. Installs the R packages: readxl, dplyr, lubridate, ggplot2, shiny, bslib,
   DT, scales and testthat.
3. Copies the project and runs `scripts/run_analysis.R` once. This checks the
   whole pipeline against the dataset and caches the results, so the
   dashboard starts in seconds.

If the build ends with `naming to docker.io/library/retailpulse`, it worked.

## 4. Run the dashboard

```bash
docker run --rm -p 3838:3838 --name retailpulse retailpulse
```

Open **http://localhost:3838** in your browser.

- The terminal shows `Listening on http://0.0.0.0:3838` once the app is ready.
- To stop it, press **Ctrl + C** in that terminal, or run
  `docker stop retailpulse` from another terminal.
- To run it in the background, add `-d`:
  `docker run -d --rm -p 3838:3838 --name retailpulse retailpulse`.
  Stop it with `docker stop retailpulse`.

### If port 3838 is already in use

Map a different port on your PC. The second number must stay 3838:

```bash
docker run --rm -p 8080:3838 retailpulse
```

Then open http://localhost:8080.

## 5. Run the tests and scripts inside the container

```bash
# Unit and integration tests (pipeline, rules, Shiny modules)
docker run --rm retailpulse Rscript tests/testthat.R

# Check the dataset against the recorded audit figures
docker run --rm retailpulse Rscript scripts/validate_data.R

# Re-run the full analysis and print the results
docker run --rm retailpulse Rscript scripts/run_analysis.R
```

## 6. Get the derived CSV files onto your PC

`run_analysis.R` writes tables such as customers, segment profiles, lapse
comparisons and the cancellation audit to `data/processed/`. To get them on
your PC, mount a folder:

```bash
# macOS / Linux
docker run --rm -v "$(pwd)/output:/app/data/processed" retailpulse Rscript scripts/run_analysis.R

# Windows PowerShell
docker run --rm -v "${PWD}/output:/app/data/processed" retailpulse Rscript scripts/run_analysis.R
```

The CSV files appear in a new `output/` folder. If you get a permission error
on Linux, add `--user "$(id -u):$(id -g)"`.

## 7. Working on the code (live edits without rebuilding)

Mount the project folder into the container. Your local files are then used
directly, and you only need to restart the container after editing.

```bash
# macOS / Linux
docker run --rm -p 3838:3838 -v "$(pwd):/app" retailpulse

# Windows PowerShell
docker run --rm -p 3838:3838 -v "${PWD}:/app" retailpulse
```

The first start with a mounted folder takes about a minute, because the app
rebuilds its results cache in your local `data/processed/`. After that, start
times are fast again.

Rebuild the image (`docker build -t retailpulse .`) whenever you want the
changes baked in, or after changing package requirements.

## 8. Troubleshooting

| Problem | Fix |
|---|---|
| `Cannot connect to the Docker daemon` | Docker Desktop isn't running. Start it and wait for "Engine running". |
| `port is already allocated` | Use a different host port (see section 4). |
| The page doesn't load straight away | Wait for `Listening on ...` in the terminal, then refresh. |
| Build fails while installing packages | Usually a network problem. Run `docker build` again; finished steps are cached. |
| Apple Silicon (M1/M2/M3) build is slow or fails | Try `docker build --platform linux/amd64 -t retailpulse .` and run with `docker run --platform linux/amd64 ...`. |
| "RetailPulse could not load its data" in the browser | `data/raw/online_retail.xlsx` is missing from the folder you built or mounted. Re-clone the repository or restore the file. |
| Running out of disk space | Remove old images with `docker image prune`. RetailPulse's image is about 1 GB. |

## 9. Removing everything

```bash
docker stop retailpulse 2>/dev/null
docker rmi retailpulse
```

## Without Docker

If you'd rather install R directly, see `README.md` sections 6 and 7.
