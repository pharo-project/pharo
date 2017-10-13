accessing
merge: aProjectRegistration
  " ... merge is done when a spec has been loaded into the image"

  self shouldBeMutable.
  aProjectRegistration validate.
  aProjectRegistration configurationProjectSpec
    ifNotNil: [ :aSpec |
      configurationProjectSpec := aSpec copy.
      configurationProjectSpec versionOrNil
        ifNotNil: [ :version | 
          "resolve symbolic versions for loaded projects"
          configurationProjectSpec versionString: version versionString ].
      baselineProjectSpec := nil.
      self versionInfo versionString: configurationProjectSpec versionString ]
    ifNil: [ 
      baselineProjectSpec := aProjectRegistration baselineProjectSpec.
      configurationProjectSpec := nil.
      self versionInfo
        versionString: baselineProjectSpec repositoryVersionString ]