
# Development Directory Information

## Sandboxes

### bundle\_parsing.Rmd

The UNcle software can export spectra from nanoDSF, SLS, and DLS
experiments as bundled .xlsx files. In these files each tab corresponds
to the data for one *uni*. This sandbox is for the development of new
import functions that handle these files. The most notable change is
that for these files, unlike e.g. the full SLS spectra export, pivoting
is not required – the data is already fairly “tidy” in long format.
