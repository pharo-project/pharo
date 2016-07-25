initialization
primeRegistryFromImage: configurationClasses baselineClasses: baselineClasses prioritizeConfiguration: prioritizeConfiguration
  "MetacelloProjectRegistration primeRegistryFromImage"

  baselineClasses
    do: [ :cl | 
      (self baselineRegistry includesKey: cl name asString)
        ifFalse: [ 
          "not registered"
          (self primeRegistryCurrentVersionFor: cl)
            ifNotNil: [ :version | 
              | projectSpec |
              projectSpec := (version project projectSpec
                name: (MetacelloScriptEngine baseNameOf: cl name asString);
                repositories: version project projectPackage repositories copy;
                yourself) asBaselineProjectSpec.
              self primeRegisterLoadedProjectSpec: projectSpec ] ] ].
  configurationClasses
    do: [ :cl | 
      (self configurationRegistry includesKey: cl name asString)
        ifFalse: [ 
          "not registered"
          (self primeRegistryCurrentVersionFor: cl)
            ifNotNil: [ :version | 
              | projectSpec repos projectPackage project |
              project := version project.
              projectPackage := project projectPackage.
              repos := nil.
              projectPackage ~~ nil
                ifTrue: [ repos := projectPackage repositories copy ].
              projectSpec := (version project projectSpec
                name: (MetacelloScriptEngine baseNameOf: cl name asString);
                className: cl name asString;
                versionString: version versionString;
                repositories: repos;
                yourself) asConfigurationProjectSpec.
              (prioritizeConfiguration
                or: [ (self primeRegistryProjectSpecRegistered: projectSpec) not ])
                ifTrue: [ 
                  "Configuration override existing registrations or nothing registered yet"
                  self primeRegisterLoadedProjectSpec: projectSpec ] ] ] ]