Class {
	#name : 'MetacelloVersionConstructor',
	#superclass : 'MetacelloAbstractVersionConstructor',
	#instVars : [
		'errorMap'
	],
	#category : 'Metacello-Core-Constructors',
	#package : 'Metacello-Core',
	#tag : 'Constructors'
}

{ #category : 'initialization' }
MetacelloVersionConstructor >> calculate: aConfig project: aProject [

	| versionMap symbolicVersionMap pragmaDict |
	self setProject: aProject.
	self configuration: aConfig.
	versionMap := Dictionary new.
	symbolicVersionMap := Dictionary new.
	self collectAllVersionsFromVersionPragmasInto: versionMap.
	pragmaDict := self extractVersionImportPragmas.
	self verifyVersionImportPragmas: pragmaDict definedIn: versionMap.
	self collectAllVersionsFromVersionImportPragmasInto: versionMap satisfiedPragmas: pragmaDict.
	self collectAllSymbolicVersionsFromVersionPragmasInto: symbolicVersionMap.
	self project map: versionMap.
	self project errorMap: self errorMap.
	self project symbolicVersionMap: symbolicVersionMap.
	self project configuration: aConfig. "now that we have a nearly complete project, we can collect the defaultSymbolicVersions, which expect the project to be fully constructed"
	self collectDefaultSymbolicVersionsFromVersionPragmasFrom: self extractDefaultSymbolicVersionPragmas into: symbolicVersionMap receiver: self configuration. "Pick up defaults from MetacelloBaseConfiguration"
	self
		collectDefaultSymbolicVersionsFromVersionPragmasFrom: self extractCommonDefaultSymbolicVersionPragmas
		into: symbolicVersionMap
		receiver: (ConfigurationOf new project: self project). "now resolive symbolicVersions defined as symbolicVersions"
	symbolicVersionMap copy keysAndValuesDo: [ :symbolic :original |
		| versionString visited |
		versionString := original.
		visited := Set new.
		[
		visited add: versionString.
		versionString isSymbol and: [ versionString ~~ #notDefined ] ] whileTrue: [
			versionString := symbolicVersionMap at: versionString ifAbsent: [ self error: 'Cannot resolve symbolic version ' , original printString ].
			(visited includes: versionString) ifTrue: [ self error: 'Loop detected resolving symbolic version ' , original printString ] ].
		symbolicVersionMap at: symbolic put: versionString ]
]

{ #category : 'private' }
MetacelloVersionConstructor >> collectAllSymbolicVersionsFromVersionPragmasInto: symbolicVersionMap [

	| defined versionPragmaDict versionString |
	versionPragmaDict := self extractSymbolicVersionPragmas.
	versionPragmaDict keysAndValuesDo: [ :versionSymbol :pragmaColl |
		defined := false.
		pragmaColl do: [ :pragma |
			defined := true.
			versionString := self resolveSymbolicVersion: versionSymbol withPragma: pragma ].
		defined ifTrue: [ versionString ifNotNil: [ symbolicVersionMap at: versionSymbol put: versionString ] ].
		self reset ]
]

{ #category : 'private' }
MetacelloVersionConstructor >> collectAllVersionsFromVersionImportPragmasInto: versionMap satisfiedPragmas: pragmaDict [

	| defined done completed count |
	done := false.
	completed := IdentitySet new.
	count := 0.
	[
	count := count + 1.
	count > 10000 ifTrue: [ self error: 'Apparent loop in import expansion' ].
	done ] whileFalse: [
		done := true.
		pragmaDict keysAndValuesDo: [ :versionString :pragmaColl |
			| versionSpec |
			versionSpec := nil.
			defined := false.
			[
			pragmaColl do: [ :pragma |
				(completed includes: pragma) ifFalse: [
					| imports |
					done := false.
					imports := pragma argumentAt: 2.
					imports
						detect: [ :importedVersion | (versionMap includesKey: importedVersion) not ]
						ifNone: [
							imports do: [ :importedVersion |
								(versionMap at: importedVersion ifAbsent: [  ]) ifNotNil: [ :version |
									defined := true.
									completed add: pragma.
									versionSpec
										ifNotNil: [ versionSpec := versionSpec mergeSpec: version spec copy ]
										ifNil: [ versionSpec := version spec copy ].
									versionSpec versionString: versionString.
									self executeOnVersionSpec: versionSpec withPragma: pragma ] ] ] ] ] ]
				on: Error
				do: [ :ex |
					(MetacelloErrorInProjectConstructionNotification versionString: versionSpec versionString exception: ex)
						ifTrue: [ ^ ex pass ]
						ifFalse: [
							self errorMap at: versionSpec versionString put: ex.

							defined := false ] ].
			defined ifTrue: [
				| version importedVersions |
				importedVersions := OrderedCollection new.
				version := versionSpec createVersion.
				pragmaColl do: [ :pragma | importedVersions addAll: (pragma argumentAt: 2) ].
				version importedVersions: importedVersions.
				self validateVersionString: versionString againstSpec: versionSpec.
				versionMap at: versionSpec versionString put: version ].
			self reset ] ]
]

{ #category : 'private' }
MetacelloVersionConstructor >> collectAllVersionsFromVersionPragmasInto: versionMap [

	| defined |
	self extractVersionPragmas keysAndValuesDo: [ :versionString :pragmaColl |
		| versionSpec |
		versionSpec := self project versionSpec.
		versionSpec versionString: versionString.
		defined := false.
		[
		pragmaColl do: [ :pragma |
			self executeOnVersionSpec: versionSpec withPragma: pragma.
			defined := true ] ]
			on: Error
			do: [ :ex |
				(MetacelloErrorInProjectConstructionNotification versionString: versionSpec versionString exception: ex)
					ifTrue: [ ^ ex pass ]
					ifFalse: [
						self errorMap at: versionSpec versionString put: ex.
						defined := false ] ].
		defined ifTrue: [
			self validateVersionString: versionString againstSpec: versionSpec.
			versionMap at: versionSpec versionString put: versionSpec createVersion ].
		self reset ]
]

{ #category : 'private' }
MetacelloVersionConstructor >> collectDefaultSymbolicVersionsFromVersionPragmasFrom: versionPragmaDict into: symbolicVersionMap receiver: receiver [

	| defined versionString |
	versionPragmaDict keysAndValuesDo: [ :versionSymbol :pragmaColl |
		defined := false.
		symbolicVersionMap at: versionSymbol ifAbsent: [ "process the defaultSymbolicVersion only if the symbolicVersion is not defined yet"
			pragmaColl do: [ :pragma |
				defined := true.
				versionString := self resolveDefaultSymbolicVersionFor: versionSymbol withPragma: pragma receiver: receiver ].
			defined ifTrue: [ versionString ifNotNil: [ symbolicVersionMap at: versionSymbol put: versionString ] ].
			self reset ] ]
]

{ #category : 'accessing' }
MetacelloVersionConstructor >> errorMap [

	^ errorMap ifNil: [ errorMap := Dictionary new ]
]

{ #category : 'private' }
MetacelloVersionConstructor >> executeOnVersionSpec: versionSpec withPragma: pragma [

	^ (pragma argumentAt: 1) = versionSpec versionString ifTrue: [
		  self evaluatePragma: pragma.
		  self project attributes do: [ :attribute |
			  (self attributeMap at: attribute ifAbsent: [  ]) ifNotNil: [ :blockList | blockList do: [ :block | self with: versionSpec during: block ] ] ] ]
]

{ #category : 'initialization' }
MetacelloVersionConstructor >> on: aConfig [

	| cacheKey |
	cacheKey := aConfig class.
	project := MetacelloPlatform current stackCacheFor: #versionConstructor at: cacheKey doing: [ :cache |
		           super on: aConfig.
		           cache at: cacheKey put: self project ].
	self setProject: project
]

{ #category : 'initialization' }
MetacelloVersionConstructor >> on: aConfig project: aProject [

	| cacheKey cachedProject |
	cacheKey := aConfig class.
	cachedProject := MetacelloPlatform current stackCacheFor: #versionConstructor at: cacheKey doing: [ :cache |
		                 super on: aConfig project: aProject.
		                 cache at: cacheKey put: self project.
		                 ^ self ].
	aProject map: cachedProject map.
	aProject errorMap: cachedProject errorMap.
	aProject symbolicVersionMap: cachedProject symbolicVersionMap.
	aProject configuration: aConfig.
	self setProject: aProject
]

{ #category : 'private' }
MetacelloVersionConstructor >> resolveDefaultSymbolicVersionFor: symbolicVrsn withPragma: pragma receiver: receiver [

	| result |
	(pragma argumentAt: 1) = symbolicVrsn ifTrue: [
		self symbolicVersion: symbolicVrsn.
		result := [ receiver perform: pragma methodSelector ]
			          on: MetacelloVersionDoesNotExistError
			          do: [ :ex | ex return: nil ] ].
	^ result
]

{ #category : 'private' }
MetacelloVersionConstructor >> resolveSymbolicVersion: symbolicVrsn withPragma: pragma [

	| result |
	(pragma argumentAt: 1) = symbolicVrsn ifTrue: [
		self symbolicVersion: symbolicVrsn.
		self evaluatePragma: pragma.
		self project attributes do: [ :attribute | (self attributeMap at: attribute ifAbsent: [  ]) ifNotNil: [ :versionString | result := versionString ] ] ].
	^ result
]

{ #category : 'validation' }
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

{ #category : 'private' }
MetacelloVersionConstructor >> verifyVersionImportPragmas: pragmaDict definedIn: versionMap [

	pragmaDict copy keysAndValuesDo: [ :versionString :pragmaColl |
		[
		pragmaColl do: [ :pragma |
			(pragma argumentAt: 2) do: [ :importedVersion |
				versionMap at: importedVersion ifAbsent: [
					pragmaDict at: importedVersion ifAbsent: [
						^ self error:
							  'The imported version:' , importedVersion printString , ' for version: ' , versionString , ' referenced from the method: '
							  , pragma methodSelector printString , ' in configuration ' , configuration class printString , ' has not been defined.' ] ] ] ] ]
			on: Error
			do: [ :ex |
				(MetacelloErrorInProjectConstructionNotification versionString: versionString exception: ex)
					ifTrue: [ ^ ex pass ]
					ifFalse: [
						pragmaDict removeKey: versionString.
						self errorMap at: versionString put: ex ] ] ]
]
