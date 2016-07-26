tests
testSemanticVersioningSubsetCompliance
    "subset of sample versions that are compatible with MetacellVersionNumber syntax"

    self
        validateSemanticVersionStrings:
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0')