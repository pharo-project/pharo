querying
packageNamed: aString forMap: map ifAbsent: absentBlock
    "import: only allowed to be used with baseline project specs"

    ^ self
        packageNamed: aString
        forLoad: true
        forMap: map
        ifAbsent: absentBlock