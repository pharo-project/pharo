api-configuration
addSectionsFrom: inputVersionStringOrSymbol forBaseline: forBaseline updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock
    | constructor updatedSpecs projectAttributes fullVersionSpec version coll pragma imports visitedSpecs fromVersionString |
    constructor := self constructor.
    updatedSpecs := Set new.
    visitedSpecs := Set new.
    projectAttributes := project attributes.
    version := project version: inputVersionStringOrSymbol.
    fromVersionString := version versionString.
    fullVersionSpec := version spec.
    fullVersionSpec := version spec.
    coll := constructor extractAllVersionPragmas at: fromVersionString ifAbsent: [ ^ nil ].
    coll size > 1
        ifTrue: [ self error: 'More than one pragma defining ' , fromVersionString printString ].
    pragma := coll at: 1.
    imports := pragma numArgs = 2
        ifTrue: [ pragma argumentAt: 2 ]
        ifFalse: [ #() ].
    methodSpec imports: imports.
    constructor extractMethodSectionsFor: fromVersionString.
    constructor methodSections
        do: [ :methodSection | 
            | versionSpec attributeList |
            versionSpec := methodSection versionSpec.
            attributeList := methodSection attributes.
            forBaseline
                ifFalse: [ 
                    (projectAttributes includes: attributeList)
                        ifTrue: [ 
                            self
                                updateVersionSpec: versionSpec
                                fullVersionSpec: fullVersionSpec
                                updateProjects: updateProjects
                                updatePackages: updatePackages
                                visited: visitedSpecs
                                updated: updatedSpecs ] ].
            (aBlock value: methodSection attributeOrPath value: versionSpec)
                ifTrue: [ self methodSpec addMethodSection: methodSection attributePath versionSpec: versionSpec ] ]