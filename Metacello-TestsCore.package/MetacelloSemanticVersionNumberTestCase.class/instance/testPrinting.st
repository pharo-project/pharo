tests
testPrinting
    | x vrsn |
    self sampleVersionStrings
        do: [ :versionString | self assert: versionString = (x := (vrsn := versionString asMetacelloSemanticVersionNumber) printString) ]