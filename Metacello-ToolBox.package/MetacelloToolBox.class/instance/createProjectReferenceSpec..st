spec creation
createProjectReferenceSpec: projectBaseName
  | projectSpec |
  projectSpec := (Metacello image
    project: projectBaseName;
    list)
    detect: [ :each | true ]
    ifNone: [ ^ self classicCreateProjectReferenceSpec: projectBaseName ].
  ^ project projectReferenceSpec
    name: projectBaseName;
    projectReference: projectSpec;
    yourself