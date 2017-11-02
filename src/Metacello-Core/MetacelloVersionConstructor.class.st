Class {
	#name : #MetacelloVersionConstructor,
	#superclass : #MetacelloAbstractVersionConstructor,
	#instVars : [
		'errorMap'
	],
	#category : 'Metacello-Core-Constructors'
}

{ #category : #'instance creation' }
MetacelloVersionConstructor class >> on: aConfig [

	^(self new)
		on: aConfig;
		yourself
]

{ #category : #'instance creation' }
MetacelloVersionConstructor class >> on: aConfig project: aProject [

	^(self new)
		on: aConfig project: aProject;
		yourself
]

{ #category : #deprecated }
MetacelloVersionConstructor class >> spawnPackageMethodIn: aConfig category: methodCategory named: newSelector sourceVersion: sourceVersionString targetVersion: targetVersionString blessing: blessing [

	self deprecated: 'see MetacelloToolBox class>>createDevelopment:for:importFromBaseline:description: for a similar example'.
	^self new
		spawnPackageMethodIn: aConfig 
		category: methodCategory 
		named: newSelector
		sourceVersion: sourceVersionString 
		targetVersion: targetVersionString
		blessing: blessing
]

{ #category : #deprecated }
MetacelloVersionConstructor class >> spawnPackageMethodIn: aConfig named: newSelector sourceVersion: sourceVersionString targetVersion: targetVersionString [

	self deprecated: 'see MetacelloToolBox class>>createDevelopment:for:importFromBaseline:description: for a similar example'.
	^self 
		spawnPackageMethodIn: aConfig 
		category: 'versions'
		named: newSelector 
		sourceVersion: sourceVersionString 
		targetVersion: targetVersionString 
		blessing: #development
]

{ #category : #deprecated }
MetacelloVersionConstructor class >> spawnPackageMethodIn: aConfig named: newSelector sourceVersion: sourceVersionString targetVersion: targetVersionString blessing: blessing [

	self deprecated: 'see MetacelloToolBox class>>createDevelopment:for:importFromBaseline:description: for a similar example'.
	^self new
		spawnPackageMethodIn: aConfig 
		category: 'versions'
		named: newSelector 
		sourceVersion: sourceVersionString 
		targetVersion: targetVersionString
		blessing: blessing
]

{ #category : #deprecated }
MetacelloVersionConstructor class >> updatePackageMethodIn: aConfig sourceVersion: sourceVersionString [

	self deprecated: 'see MetacelloToolBox class>>updateDevelopment:for:updateProjects:description: for a similar example'.
	^self new
		updatePackageMethodIn: aConfig 
		sourceVersion: sourceVersionString
]

{ #category : #initialization }
MetacelloVersionConstructor >> calculate: aConfig project: aProject [
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
]

{ #category : #private }
MetacelloVersionConstructor >> collectAllSymbolicVersionsFromVersionPragmasInto: symbolicVersionMap using: executionBlock [
	| defined versionPragmaDict versionString |
	versionPragmaDict := self extractSymbolicVersionPragmas.
	versionPragmaDict
		keysAndValuesDo: [ :versionSymbol :pragmaColl | 
			defined := false.
			pragmaColl
				do: [ :pragma | 
					defined := true.
					versionString := executionBlock value: versionSymbol value: pragma ].
			defined
				ifTrue: [ 
					versionString == nil
						ifFalse: [ symbolicVersionMap at: versionSymbol put: versionString ]].
			self reset ]
]

{ #category : #private }
MetacelloVersionConstructor >> collectAllVersionsFromVersionImportPragmasInto: versionMap using: executionBlock satisfiedPragmas: pragmaDict [
    | defined done completed count |
    done := false.
    completed := IdentitySet new.
    count := 0.
    [ 
    count := count + 1.
    count > 10000
        ifTrue: [ self error: 'Apparent loop in import expansion' ].
    done ]
        whileFalse: [ 
            done := true.
            pragmaDict
                keysAndValuesDo: [ :versionString :pragmaColl | 
                    | versionSpec |
                    versionSpec := nil.
                    defined := false.
                    [ 
                    pragmaColl
                        do: [ :pragma | 
                            (completed includes: pragma)
                                ifFalse: [ 
                                    | imports |
                                    done := false.
                                    imports := pragma argumentAt: 2.
                                    imports
                                        detect: [ :importedVersion | (versionMap includesKey: importedVersion) not ]
                                        ifNone: [ 
                                            imports
                                                do: [ :importedVersion | 
                                                    | version |
                                                    (version := versionMap at: importedVersion ifAbsent: [  ]) ~~ nil
                                                        ifTrue: [ 
                                                            defined := true.
                                                            completed add: pragma.
                                                            versionSpec == nil
                                                                ifTrue: [ versionSpec := version spec copy ]
                                                                ifFalse: [ versionSpec := versionSpec mergeSpec: version spec copy ].
                                                            versionSpec versionString: versionString.
                                                            executionBlock value: versionSpec value: pragma ] ] ] ] ] ]
                        on: Error
                        do: [ :ex | 
                            (MetacelloErrorInProjectConstructionNotification versionString: versionSpec versionString exception: ex)
                                ifTrue: [ ^ ex pass ]
                                ifFalse: [ 
                                    self errorMap at: versionSpec versionString put: ex.
                                 
                                    defined := false ] ].
                    defined
                        ifTrue: [ 
                            | version importedVersions |
                            importedVersions := OrderedCollection new.
                            version := versionSpec createVersion.
                            pragmaColl do: [ :pragma | importedVersions addAll: (pragma argumentAt: 2) ].
                            version importedVersions: importedVersions.
                            self validateVersionString: versionString againstSpec: versionSpec.
                            versionMap at: versionSpec versionString put: version ].
                    self reset ] ]
]

{ #category : #private }
MetacelloVersionConstructor >> collectAllVersionsFromVersionPragmasInto: versionMap using: executionBlock [
    | defined versionPragmaDict |
    versionPragmaDict := self extractVersionPragmas.
    versionPragmaDict
        keysAndValuesDo: [ :versionString :pragmaColl | 
            | versionSpec |
            versionSpec := self project versionSpec.
            versionSpec versionString: versionString.
            defined := false.
            [ 
            pragmaColl
                do: [ :pragma | 
                    executionBlock value: versionSpec value: pragma.
                    defined := true ] ]
                on: Error
                do: [ :ex | 
                    (MetacelloErrorInProjectConstructionNotification versionString: versionSpec versionString exception: ex)
                        ifTrue: [ ^ ex pass ]
                        ifFalse: [ 
                            self errorMap at: versionSpec versionString put: ex.
                            defined := false ] ].
            defined
                ifTrue: [ 
                    self validateVersionString: versionString againstSpec: versionSpec.
                    versionMap at: versionSpec versionString put: versionSpec createVersion ].
            self reset ]
]

{ #category : #private }
MetacelloVersionConstructor >> collectDefaultSymbolicVersionsFromVersionPragmasFrom: versionPragmaDict into: symbolicVersionMap using: executionBlock [
	| defined versionString |
	versionPragmaDict
		keysAndValuesDo: [ :versionSymbol :pragmaColl | 
			defined := false.
			symbolicVersionMap
				at: versionSymbol
				ifAbsent: [ 
					"process the defaultSymbolicVersion only if the symbolicVersion is not defined yet"
					pragmaColl
						do: [ :pragma | 
							defined := true.
							versionString := executionBlock value: versionSymbol value: pragma ].
					defined
						ifTrue: [ 
							versionString == nil
								ifFalse: [ symbolicVersionMap at: versionSymbol put: versionString ] ].
					self reset ] ]
]

{ #category : #private }
MetacelloVersionConstructor >> commonDefaultSymbolicVersionResolverBlock [
	^ self defaultSymbolicVersionResolverBlock: (ConfigurationOf new project: self project)
]

{ #category : #private }
MetacelloVersionConstructor >> defaultSymbolicVersionResolverBlock [
	^ self defaultSymbolicVersionResolverBlock: self configuration
]

{ #category : #private }
MetacelloVersionConstructor >> defaultSymbolicVersionResolverBlock: receiver [
	^ [ :symbolicVrsn :pragma | 
	| result |
	result := nil.
	(pragma argumentAt: 1) = symbolicVrsn
		ifTrue: [ 
			self symbolicVersion: symbolicVrsn.
			result := [ receiver perform: (MetacelloPlatform current selectorForPragma: pragma) ] 
				on: MetacelloVersionDoesNotExistError 
				do: [ :ex | ex return: nil ] ].
	result ]
]

{ #category : #accessing }
MetacelloVersionConstructor >> errorMap [
    errorMap ifNil: [ errorMap := Dictionary new ].
    ^ errorMap
]

{ #category : #initialization }
MetacelloVersionConstructor >> on: aConfig [

	| cacheKey |
	cacheKey := aConfig class.
	project := MetacelloPlatform current
		stackCacheFor: #versionConstructor
		at: cacheKey
		doing: [ :cache | 
			self calculate: aConfig project: nil.
			cache at: cacheKey put:  self project ].
	self setProject: project.
]

{ #category : #initialization }
MetacelloVersionConstructor >> on: aConfig project: aProject [
    | cacheKey cachedProject |
    cacheKey := aConfig class.
    cachedProject := MetacelloPlatform current
        stackCacheFor: #'versionConstructor'
        at: cacheKey
        doing: [ :cache | 
            self calculate: aConfig project: aProject.
            cache at: cacheKey put: self project.
            ^ self ].
    aProject map: cachedProject map.
    aProject errorMap: cachedProject errorMap.
    aProject symbolicVersionMap: cachedProject symbolicVersionMap.
    aProject configuration: aConfig.
    self setProject: aProject
]

{ #category : #deprecated }
MetacelloVersionConstructor >> prepareForMethodUpdate: aConfig sourceVersion: sourceVersionString forceUpdate: forceUpdate generating: generateBlock [
	| pragmaDict versionSpecs pragmaMap updatedPackageSpecs updatedPackageSpecsMap reversed pragmaColl seenUpdatedPackageSpecs |
	self deprecated: 'see MetacelloToolBox for replacement methods'.
	self configuration: aConfig.
	versionSpecs := Dictionary new.
	pragmaMap := Dictionary new.
	pragmaDict := self extractAllVersionPragmas.
	pragmaColl := pragmaDict at: sourceVersionString ifAbsent: [ ^ self ].
	pragmaColl
		do: [ :pragma | 
			| specs versionSpec |
			specs := Dictionary new.
			self evaluatePragma: pragma.
			self attributeMap
				keysAndValuesDo: [ :attribute :blockList | 
					versionSpec := self project versionSpec.
					versionSpec versionString: sourceVersionString.
					specs at: attribute put: versionSpec.
					blockList do: [ :block | self with: versionSpec during: block ] ].
			versionSpecs
				at: pragma selector
				put:
					{specs.
					(self attributeOrder).
					nil}.
			pragmaMap at: pragma selector put: pragma.
			self reset ].
	versionSpecs
		keysAndValuesDo: [ :selector :ar | 
			updatedPackageSpecsMap := Dictionary new.
			seenUpdatedPackageSpecs := Dictionary new.
			ar at: 3 put: updatedPackageSpecsMap.
			reversed := aConfig project attributes reverse.
			reversed
				do: [ :attribute | 
					| vs |
					(vs := (ar at: 1) at: attribute ifAbsent: [  ]) ~~ nil
						ifTrue: [ 
							updatedPackageSpecs := forceUpdate
								ifTrue: [ vs forceUpdatedPackageSpecs ]
								ifFalse: [ vs updatedPackageSpecs ].
							updatedPackageSpecs associations
								do: [ :assoc | 
									| filename |
									(filename := seenUpdatedPackageSpecs at: assoc key ifAbsent: [  ]) == nil
										ifTrue: [ 
											assoc value == #uptodate
												ifTrue: [ 
													"#uptodate means that the spec is up-to-date and we mark it as
										 seen so that the spec is not update for a 'later' spec"
													seenUpdatedPackageSpecs at: assoc key put: #uptodate ]
												ifFalse: [ seenUpdatedPackageSpecs at: assoc key put: assoc value file ] ]
										ifFalse: [ 
											"if the spec was already seen as up-to-date or the file is the same as 
								 the one already seen don't propogate the file"
											(filename == #uptodate or: [ assoc value == #uptodate or: [ filename = assoc value file ] ])
												ifTrue: [ updatedPackageSpecs removeKey: assoc key ] ] ].
							updatedPackageSpecsMap at: attribute put: updatedPackageSpecs ] ].	"clear out #uptodate markers"
			updatedPackageSpecsMap
				valuesDo: [ :d | 
					d associations
						do: [ :assoc | 
							assoc value == #uptodate
								ifTrue: [ d removeKey: assoc key ] ] ].	"remove shadowed packages"
			1 to: reversed size do: [ :index | 
				| attribute d |
				attribute := reversed at: index.
				((d := updatedPackageSpecsMap at: attribute ifAbsent: [  ]) ~~ nil and: [ d keys size > 0 ])
					ifTrue: [ 
						index + 1 to: reversed size do: [ :shadowIndex | 
							d
								keysDo: [ :key | 
									| dict |
									(dict := updatedPackageSpecsMap at: (reversed at: shadowIndex) ifAbsent: [  ]) ~~ nil
										ifTrue: [ dict removeKey: key ifAbsent: [  ] ] ] ] ] ].
			updatedPackageSpecsMap keys
				do: [ :key | 
					| d |
					d := updatedPackageSpecsMap at: key.
					d isEmpty
						ifTrue: [ updatedPackageSpecsMap removeKey: key ] ].	"ready to generate source for method"
			generateBlock
				value: selector
				value: pragmaMap
				value: ar
				value: updatedPackageSpecsMap ]
]

{ #category : #deprecated }
MetacelloVersionConstructor >> spawnPackageMethodIn: aConfig category: methodCategory named: newSelector sourceVersion: sourceVersionString targetVersion: targetVersionString blessing: blessing [

	self deprecated: 'see MetacelloToolBox class>>createBaseline:for:from:description: for replacement method'.
	self 
		prepareForMethodUpdate: aConfig 
		sourceVersion: sourceVersionString 
		forceUpdate: blessing ~~ #baseline
		generating: [:selector :pragmaMap :ar :updatedPackageSpecsMap | | strm |
			strm := WriteStream on: String new.
			strm 
				nextPutAll: newSelector asString, ' spec'; cr;
				tab; nextPutAll: 
						'<version: ', 
						targetVersionString printString, 
						' imports: #(', sourceVersionString printString, 
						')>';cr.
			(ar at: 2) do: [:attribute | | vs d |
				vs := (ar at: 1) at: attribute.
				(d := updatedPackageSpecsMap at: attribute ifAbsent: []) ~~ nil
					ifTrue: [ 
						vs packagesSpec list do: [:member | | x |
							x := d at: member spec name ifAbsent: [].
						 	member spec updateForSpawnMethod: x ]]
					ifFalse: [
						vs packagesSpec list do: [:member |
							member spec updateForSpawnMethod: member spec copy ]].
				strm cr; tab; nextPutAll: 'spec for: ', attribute printString, ' do: ['; cr.
				attribute == #common
					ifTrue: [
						vs 
							author: MetacelloPlatform current authorName;
							timestamp: MetacelloPlatform current timestamp.
						blessing ~~ nil
							ifTrue: [ vs blessing: blessing ]].
				vs configSpawnMethodOn: strm indent: 2.
				strm nextPutAll: '].'].
				(aConfig class	
					compile: strm contents
					classified: methodCategory) == nil 
						ifTrue: [ self error: 'Error compiling the method' ]].
	^true
]

{ #category : #private }
MetacelloVersionConstructor >> specResolverBlock [
	^ [ :versionSpec :pragma | 
	(pragma argumentAt: 1) = versionSpec versionString
		ifTrue: [ 
			self evaluatePragma: pragma.
			self project attributes
				do: [ :attribute | 
					| blockList |
					(blockList := self attributeMap at: attribute ifAbsent: [  ]) ~~ nil
						ifTrue: [ blockList do: [ :block | self with: versionSpec during: block ] ] ] ] ]
]

{ #category : #private }
MetacelloVersionConstructor >> symbolicVersionResolverBlock [
	^ [ :symbolicVrsn :pragma | 
	| result |
	result := nil.
	(pragma argumentAt: 1) = symbolicVrsn
		ifTrue: [ 
			self symbolicVersion: symbolicVrsn.
			self evaluatePragma: pragma.
			self project attributes
				do: [ :attribute | 
					| versionString |
					versionString := self attributeMap at: attribute ifAbsent: [  ].
					versionString ~~ nil
						ifTrue: [ result := versionString ] ] ].
	result ]
]

{ #category : #deprecated }
MetacelloVersionConstructor >> updatePackageMethodIn: aConfig sourceVersion: sourceVersionString [

	self deprecated: 'see MetacelloToolBox class>>updateDevelopment:for:updateProjects:description: for replacement method'.
	self 
		prepareForMethodUpdate: aConfig 
		sourceVersion: sourceVersionString 
		forceUpdate: false
		generating: [:selector :pragmaMap :ar :updatedPackageSpecsMap | | strm pragma |
			updatedPackageSpecsMap isEmpty ifTrue: [ ^false ].
			strm := WriteStream on: String new.
			strm 
				nextPutAll: selector asString, ' spec'; cr;
				tab; nextPutAll: '<version: ', sourceVersionString printString.
			pragma := pragmaMap at: selector.
			pragma numArgs = 2
				ifTrue: [
					strm nextPutAll: ' imports: #('.
					(pragma argumentAt: 2) do: [:versionString |
						strm nextPutAll: versionString printString; space ]].
			strm nextPutAll: ')>';cr.
			(ar at: 2) do: [:attribute | | vs d |
				vs := (ar at: 1) at: attribute.
				(d := updatedPackageSpecsMap at: attribute ifAbsent: []) ~~ nil
					ifTrue: [ 
						vs packagesSpec list do: [:member | 
							member spec file ~~ nil
								ifTrue: [ | x |
									(((x := d at: member spec name ifAbsent: []) ~~ nil) and: [ x ~~ #uptodate ])
						 				ifTrue: [ member spec file: x file ]]]].
				strm cr; tab; nextPutAll: 'spec for: ', attribute printString, ' do: ['; cr.
				attribute == #common
					ifTrue: [ 
						vs 
							author: MetacelloPlatform current authorName;
							timestamp: MetacelloPlatform current timestamp ].
				vs configMethodOn: strm indent: 2.
				strm nextPutAll: '].'].
			(aConfig class	
					compile: strm contents
					classified: (aConfig class whichCategoryIncludesSelector: pragma selector)) == nil
						ifTrue: [ self error: 'Error compiling the method' ]].
	^true
]

{ #category : #validation }
MetacelloVersionConstructor >> validateVersionString: versionString againstSpec: versionSpec [

	versionString = versionSpec versionString
		ifFalse: [ 
			MetacelloValidationNotification
				signal:
					(MetacelloValidationError
						configurationClass: self configurationClass
						reasonCode: #incorrectVersionString
						callSite: #validateVersionString:againstSpec
						explanation:
							'The version declared in the pragma ', versionString printString , ' does not match the version in the spec '
								, versionSpec versionString printString) ].
]

{ #category : #private }
MetacelloVersionConstructor >> verifyVersionImportPragmas: pragmaDict definedIn: versionMap [
    pragmaDict copy
        keysAndValuesDo: [ :versionString :pragmaColl | 
            [ 
            pragmaColl
                do: [ :pragma | 
                    (pragma argumentAt: 2)
                        do: [ :importedVersion | 
                            versionMap
                                at: importedVersion
                                ifAbsent: [ 
                                    pragmaDict
                                        at: importedVersion
                                        ifAbsent: [ 
                                            ^ self
                                                error:
                                                    'The imported version:' , importedVersion printString , ' for version: ' , versionString
                                                        , ' referenced from the method: ' , (MetacelloPlatform current selectorForPragma: pragma) printString
                                                        , ' in configuration ' , configuration class printString
                                                        , ' has not been defined.' ] ] ] ] ]
                on: Error
                do: [ :ex | 
                    (MetacelloErrorInProjectConstructionNotification versionString: versionString exception: ex)
                        ifTrue: [ ^ ex pass ]
                        ifFalse: [ 
                            pragmaDict removeKey: versionString.
                            self errorMap at: versionString put: ex ] ] ]
]
