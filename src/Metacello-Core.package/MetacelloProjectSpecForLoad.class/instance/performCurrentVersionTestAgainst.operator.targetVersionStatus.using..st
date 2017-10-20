operations
performCurrentVersionTestAgainst: vrsn operator: anOperator targetVersionStatus: targetVersionStatus using: anMCLoader
  "answer true if the current state of image is to be left as is"

  | currentVersion existing new |
  vrsn ifNil: [ ^ false ].
  self useDetermineVersionForLoad
    ifTrue: [ 
      | prjct cv |
      self hasOverride
        ifTrue: [ 
          self
            error:
              'unexpected logic combination: useDeterminVersionForLoad & hasOverride' ].
      prjct := self projectSpec projectClassProject.
      prjct loader: anMCLoader.
      (cv := prjct currentVersion) == nil
        ifTrue: [ ^ false ].
      (targetVersionStatus includes: cv versionStatus)
        ifTrue: [ ^ cv perform: anOperator with: vrsn ].
      ^ false ].
  true
    ifTrue: [ 
      "unconditional fail test at this point for Issue #388"
      "https://github.com/dalehenrich/metacello-work/issues/388"
      ^ false ].
  (self hasOverride not or: [ targetVersionStatus ~= #(#'allLoadedToSpec') ])
    ifTrue: [ ^ false ].
  (self overrideProjectSpec allPackagesLoaded: anMCLoader)
    ifFalse: [ 
      "roughly equivalent to versionStatus test above (#'allLoadedToSpec')"
      ^ false ].
  (self overrideProjectSpec
    isPartiallyLoaded: self overrideProjectSpec copy loader)
    ifFalse: [ 
      "if the project is not loaded at all"
      ^ false ].
  (currentVersion := self overrideProjectSpec versionOrNil) ifNil: [ ^ false ].
  currentVersion = vrsn
    ifTrue: [ ^ true ].
  existing := self overrideProjectSpec asProjectRegistration.
  new := self projectSpec asProjectRegistration.
  ^ currentVersion > vrsn
    ifTrue: [ 
      "answer false if downgrade allowed"
      (MetacelloAllowProjectDowngrade new
        existingProjectRegistration: existing;
        newProjectRegistration: new;
        signal) == existing ]
    ifFalse: [ 
      "answer false if upgrade allowed"
      (MetacelloAllowProjectUpgrade new
        existingProjectRegistration: existing;
        newProjectRegistration: new;
        signal) == existing ]