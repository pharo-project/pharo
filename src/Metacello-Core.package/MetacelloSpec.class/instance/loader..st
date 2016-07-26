accessing
loader: aLoader
    "We're interested in propogating the loader state, _except_ for the spec"

    self shouldBeMutable.
    loader := aLoader copy.
    loader spec: self