initialization
calculate: aConfig project: aProject
    | versionMap symbolicVersionMap executionBlock pragmaDict |
    self setProject: aProject.
    self configuration: aConfig.
    versionMap := Dictionary new.
    symbolicVersionMap := Dictionary new.
    executionBlock := self specResolverBlock.
    self collectAllVersionsFromVersionPragmasInto: versionMap using: executionBlock.
    pragmaDict := self extractVersionImportPragmas.
    self verifyVersionImportPragmas: pragmaDict definedIn: versionMap.
    self collectAllVersionsFromVersionImportPragmasInto: versionMap using: executionBlock satisfiedPragmas: pragmaDict.
    self collectAllSymbolicVersionsFromVersionPragmasInto: symbolicVersionMap using: self symbolicVersionResolverBlock.
    self project map: versionMap.
    self project errorMap: self errorMap.
    self project symbolicVersionMap: symbolicVersionMap.
    self project configuration: aConfig.	"now that we have a nearly complete project, we can collect the defaultSymbolicVersions, which expect the project to be fully constructed"
    self
        collectDefaultSymbolicVersionsFromVersionPragmasFrom: self extractDefaultSymbolicVersionPragmas
        into: symbolicVersionMap
        using: self defaultSymbolicVersionResolverBlock.	"Pick up defaults from MetacelloBaseConfiguration"
    self
        collectDefaultSymbolicVersionsFromVersionPragmasFrom: self extractCommonDefaultSymbolicVersionPragmas
        into: symbolicVersionMap
        using: self commonDefaultSymbolicVersionResolverBlock.	"now resolive symbolicVersions defined as symbolicVersions"
    symbolicVersionMap copy
        keysAndValuesDo: [ :symbolic :original | 
            | versionString visited |
            versionString := original.
            visited := Set new.
            [ 
            visited add: versionString.
            versionString isSymbol and: [ versionString ~~ #'notDefined' ] ]
                whileTrue: [ 
                    versionString := symbolicVersionMap
                        at: versionString
                        ifAbsent: [ self error: 'Cannot resolve symbolic version ' , original printString ].
                    (visited includes: versionString)
                        ifTrue: [ self error: 'Loop detected resolving symbolic version ' , original printString ] ].
            symbolicVersionMap at: symbolic put: versionString ]