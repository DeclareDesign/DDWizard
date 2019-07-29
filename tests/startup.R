app <- ShinyDriver$new("../", seed = 1234)
app$snapshotInit("startup")

app$snapshot()
