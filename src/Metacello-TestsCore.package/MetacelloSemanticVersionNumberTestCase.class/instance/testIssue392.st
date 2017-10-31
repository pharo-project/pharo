tests
testIssue392
  "semantic version strings that are actually invalid, but were allowed by MetacelloOldSemanticVersionNumber"

  #('v0.0.1' '00.0.1' '16r00.0.1' '101r.0.1' 'rr101r.0.1')
    do: [ :versionString | 
      MetacelloOldSemanticVersionNumber fromString: versionString.
      self should: [ self versionClass fromString: versionString ] raise: Error ]