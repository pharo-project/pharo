test matching
testInvalidSpecialMatch
    self should: [ (self versionClass fromString: '1.1.1') match: '?.?.?' ] raise: Error