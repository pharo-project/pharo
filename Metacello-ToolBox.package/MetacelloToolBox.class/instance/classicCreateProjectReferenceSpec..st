spec creation
classicCreateProjectReferenceSpec: projectBaseName
  | configurationName projectReferenceProject versionString |
  configurationName := self configurationNameFrom: projectBaseName.
  projectReferenceProject := (MetacelloPlatform current
    globalNamed: configurationName asSymbol
    ifAbsent: [ 
      self
        error:
          'The project with configuration ' , configurationName printString
            , ' for the project ' , projectBaseName printString
            , ' is not currently loaded into the image.' ])
    project.
  versionString := (projectReferenceProject hasVersion: #'stable')
    ifTrue: [ #'stable' ]
    ifFalse: [ #'bleedingEdge' ].
  ^ project projectReferenceSpec
    name: projectBaseName;
    projectReference:
        (project projectSpec
            name: projectBaseName;
            className: configurationName;
            versionString: versionString;
            yourself);
    yourself