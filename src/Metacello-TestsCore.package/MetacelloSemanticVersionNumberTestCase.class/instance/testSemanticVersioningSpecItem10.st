tests
testSemanticVersioningSpecItem10
    "[Semantic Versioning 2.0.0-rc.1](http://semver.org/)"

    self validateSemanticVersionStrings: #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-0.3.7' '1.0.0-x.7.z.92')