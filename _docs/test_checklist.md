# Checklist for testing the app

Since automatted testing (of the interface) doesn't really work (see details in `README.md`), I propose the following checklist that should be used before committing major changes:


- [ ] load all designers in design tab, open at least the "source" box and switch to inspect tab one by one
- [ ] for random designers, change values in design tab
    - make sure to include vector inputs
    - also try input errors (e.g. "1, foo, 2" in a numeric vector input)
    - randomly fix/unfix an argument
    - check "read more" button
- [ ] for random designers, change values in inspect tab
    - make sure to include lists of vectors
    - also try input errors (e.g. "1, foo, 2" in a numeric vector input)
    - randomly fix/unfix an argument
- [ ] for a random designer and random inputs:
    - download the R code and the RDS file
    - redraw the simulated data and download it
- [ ] for a random designer and random inputs, diagnose a design
    - randomly change plot config.
    - download plot
    - download plot code
    - show diagnosis box
    - download reduced and full diagnosis results
- [ ] for random designer and random inputs create a bookmark and restore it in a different browser window
- [ ] help, legal notice, data protection policy buttons
