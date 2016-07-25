enumerating
allChildrenMatching: aPattern
    "Return all the files and folders recursively nested in the receiver and matching the pattern, aPattern"
    
    ^ self allChildren select: [ :each |  aPattern match: each basename ]
