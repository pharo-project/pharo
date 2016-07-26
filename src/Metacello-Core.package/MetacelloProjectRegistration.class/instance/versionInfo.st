accessing
versionInfo
  versionInfo
    ifNil: [ 
      versionInfo := MetacelloProjectRegistrationVersionInfo new.
      self
        configurationProjectSpecIfPresent: [ versionInfo versionString: self version ]
        ifAbsent: [ 
          "do not set versionString for freshly minted baseline ... 
           see https://github.com/dalehenrich/metacello-work/issues/328"
           ] ].
  ^ versionInfo