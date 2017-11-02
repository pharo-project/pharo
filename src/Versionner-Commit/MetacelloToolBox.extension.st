Extension { #name : #MetacelloToolBox }

{ #category : #'*Versionner-Commit' }
MetacelloToolBox class >> createFixedDevelopment: developmentVersionString for: configurationBasename importFromBaseline: baselineVersionString description: aString [
	"Create a new development version based on the specifications in an existing baseline version. Modify the #development symbolic version to reference the new version."

	<apiDocumentation>
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		createVersionMethod: 'version' , (self stripVersionStringForSelector: developmentVersionString) , ':'
			inCategory: 'versions'
			forVersion: developmentVersionString;
		fixedImportFrom: baselineVersionString
			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, blessing, description and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							blessing: #development;
							description: aString;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
		commitMethod.
	toolbox
		symbolicVersionMethod: #development;
		addSymbolicSection: #common version: developmentVersionString;
		commitMethod.
	^ toolbox
]

{ #category : #'*Versionner-Commit' }
MetacelloToolBox class >> createFixedVersion: targetVersionString for: configurationBasename from: sourceVersionString description: aString [
	"Create a new version based on the development version.
	 I'm assuming Versionner style: development versions are baselines"
	<apiDocumentation>
	| toolbox sourceVersion |
	toolbox := self configurationNamed: configurationBasename.
	sourceVersion := toolbox project version: sourceVersionString.
	(targetVersionString indexOfSubCollection: 'baseline') > 0 
		ifTrue: [ self error: '' ].

	"create development version version from development version"
	self
		createFixedDevelopment: targetVersionString
		for: configurationBasename
		importFromBaseline: sourceVersion versionString
		description: aString.

]

{ #category : #'*Versionner-Commit' }
MetacelloToolBox >> fixedEditVersionSpecsForImport: constructor projectAttributes: projectAttributes active: activeSection fullVersionSpec: fullVersionSpec updateProjects: updateProjects updatePackages: updatePackages visited: visitedSpecs updated: updatedSpecs [
    | fileSpecified encounteredPackageSpecs |
    fileSpecified := Bag new.
    encounteredPackageSpecs := Bag new.
    constructor
        methodSectionsInEvaluationOrder: projectAttributes reverse
        do: [ :methodSection | 
            "collect packageSpec names ... interested in singletons"
            methodSection versionSpec
                specListProjectDo: [ :spec |  ]
                packageDo: [ :spec | 
                    encounteredPackageSpecs add: spec name.
                    (spec copy updateForSpawnMethod: spec)
                        ifFalse: [ fileSpecified add: spec name ] ]
                groupDo: [ :spec |  ] ].
    constructor
        methodSectionsInEvaluationOrder: projectAttributes reverse
        do: [ :methodSection | 
            | versionSpec attributePath packageSpecsToDelete |
            versionSpec := methodSection versionSpec.
            attributePath := methodSection attributePath.
            versionSpec updateForSpawnMethod: versionSpec.
            packageSpecsToDelete := OrderedCollection new.
            versionSpec
                specListProjectDo: [ :spec | spec fixedUpdateForSpawnMethod: spec ]
                packageDo: [ :spec | 
                    encounteredPackageSpecs remove: spec name.
                    (spec updateForSpawnMethod: spec)
                        ifTrue: [ 
                            "no file speced"
                            ((fileSpecified includes: spec name) or: [ encounteredPackageSpecs includes: spec name ])
                                ifTrue: [ 
                                    "already specified somewhere else, need to delete this spec from version spec OR we're deleting all but first occurence"
                                    packageSpecsToDelete add: spec ]
                                ifFalse: [ 
                                    "file hasn't been specified, so specify now"
                                    spec file: spec name.
                                    fileSpecified add: spec name ] ]
                        ifFalse: [ 
                            "file specified"
                            (fileSpecified occurrencesOf: spec name) > 1
                                ifTrue: [ 
                                    "multiple specifications for file ... delete all but first occurence"
                                    packageSpecsToDelete add: spec.
                                    fileSpecified remove: spec name ] ] ]
                groupDo: [ :spec | spec updateForSpawnMethod: spec ].
            packageSpecsToDelete do: [ :spec | versionSpec deleteSpec: spec ].
            activeSection
                ifTrue: [ 
                    self
                        updateVersionSpec: versionSpec
                        fullVersionSpec: fullVersionSpec
                        updateProjects: updateProjects
                        updatePackages: updatePackages
                        visited: visitedSpecs
                        updated: updatedSpecs ]
                ifFalse: [ 
                    self
                        clearVersionSpec: versionSpec
                        fullVersionSpec: fullVersionSpec
                        updateProjects: updateProjects
                        updatePackages: updatePackages ] ]
]

{ #category : #'*Versionner-Commit' }
MetacelloToolBox >> fixedImportFrom: inputVersionStringOrSymbol projectAttributes: projectAttributes inactiveAttributes: inactiveAttributesList updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock [
    | constructor updatedSpecs fullVersionSpec version visitedSpecs fromVersionString attributeSet |
    constructor := self constructor.
    updatedSpecs := Set new.
    visitedSpecs := Set new.
    version := project version: inputVersionStringOrSymbol.
    fromVersionString := version versionString.	"resolve symbolic versions"
    fullVersionSpec := version spec.
    self methodSpec imports: {fromVersionString}.
    constructor extractMethodSectionsFor: fromVersionString.
    self
        fixedEditVersionSpecsForImport: constructor
        projectAttributes: projectAttributes
        active: true
        fullVersionSpec: fullVersionSpec
        updateProjects: updateProjects
        updatePackages: updatePackages
        visited: visitedSpecs
        updated: updatedSpecs.
    attributeSet := projectAttributes.
    inactiveAttributesList
        do: [ :inactiveAttributes | 
            self
                editVersionSpecsForImport: constructor
                projectAttributes: inactiveAttributes
                active: false
                fullVersionSpec: fullVersionSpec
                updateProjects: updateProjects
                updatePackages: updatePackages
                visited: visitedSpecs
                updated: updatedSpecs.
            attributeSet := attributeSet , projectAttributes ].
    attributeSet := attributeSet asSet.
    constructor
        methodSectionsDo: [ :methodSection | 
            | versionSpec attributePath |
            versionSpec := methodSection versionSpec.
            attributePath := methodSection attributePath.
            (methodSection includesAttributeFrom: attributeSet)
                ifFalse: [ 
                    versionSpec updateForSpawnMethod: versionSpec.
                    versionSpec
                        specListProjectDo: [ :spec | spec fixedUpdateForSpawnMethod: spec ]
                        packageDo: [ :spec | spec updateForSpawnMethod: spec ]
                        groupDo: [ :spec | spec updateForSpawnMethod: spec ].
                    self
                        clearVersionSpec: versionSpec
                        fullVersionSpec: fullVersionSpec
                        updateProjects: updateProjects
                        updatePackages: updatePackages ].
            (aBlock value: attributePath asAttributeOrPath value: versionSpec)
                ifTrue: [ self methodSpec addMethodSection: attributePath versionSpec: versionSpec ] ]
]

{ #category : #'*Versionner-Commit' }
MetacelloToolBox >> fixedImportFrom: inputVersionStringOrSymbol updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock [
    ^ self
        fixedImportFrom: inputVersionStringOrSymbol
        projectAttributes: project attributes
        inactiveAttributes: #()
        updateProjects: updateProjects
        updatePackages: updatePackages
        versionSpecsDo: aBlock
]
