tests
testLessThanComparison
    self assert: '1.0.0-rc.1+build.1' asMetacelloSemanticVersionNumber < '1.0.0' asMetacelloSemanticVersionNumber.
    self
        assert: '1.0.0-rc.1+build.1' asMetacelloSemanticVersionNumber < '1.0.0+build.0' asMetacelloSemanticVersionNumber.
    self assert: '1.0.0-alpha.1' asMetacelloSemanticVersionNumber < '1.0.0-0.3.7' asMetacelloSemanticVersionNumber.
    self assert: '1.0.0-alpha' asMetacelloSemanticVersionNumber < '1.0.0-alpha.1' asMetacelloSemanticVersionNumber.
    self assert: '1.0.0+-' asMetacelloSemanticVersionNumber < '1.0.0+a' asMetacelloSemanticVersionNumber.
    self assert: '1.0.0-0.3.7' asMetacelloSemanticVersionNumber < '1.0.0-x.7.z.92' asMetacelloSemanticVersionNumber