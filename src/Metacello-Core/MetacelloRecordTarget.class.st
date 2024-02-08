Class {
	#name : 'MetacelloRecordTarget',
	#superclass : 'MetacelloTarget',
	#instVars : [
		'required',
		'currentRepositorySpecs'
	],
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'accessing' }
MetacelloRecordTarget >> actionLabel [

	^'Recording '
]

{ #category : 'private' }
MetacelloRecordTarget >> atomicLoadPackages: packages ofSpec: spec [

	self specBuilder
		pushAtomicLoadDirectivesDuring: [
			self specBuilder currentLoadDirective spec: spec.
			packages do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'accessing' }
MetacelloRecordTarget >> currentRepositorySpecs: aCollection [ 
	currentRepositorySpecs := aCollection
]

{ #category : 'private' }
MetacelloRecordTarget >> linearLoadPackages: packages ofSpec: spec [

	self specBuilder
		pushLinearLoadDirectivesDuring: [
			self specBuilder currentLoadDirective spec: spec.
			packages do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'doits' }
MetacelloRecordTarget >> postLoad: packageOrVersionSpec [

	(MetacelloDirective postLoadSpec: packageOrVersionSpec) addTo:
		self specBuilder currentLoadDirective
]

{ #category : 'doits' }
MetacelloRecordTarget >> preLoad: packageOrVersionSpec [

	(MetacelloDirective preLoadSpec: packageOrVersionSpec) addTo:
		self specBuilder currentLoadDirective
]

{ #category : 'accessing' }
MetacelloRecordTarget >> required: aCollection [ 
	
	required := aCollection
]

{ #category : 'private' }
MetacelloRecordTarget >> specBuilder [
		
	^ engine specBuilder
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitGroupSpec: aMetacelloGroupSpec [ 
	
	"Nothing"
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitPackageSpec: aMetacelloPackageSpec [

	| directive packageRepositorySpecs |
	packageRepositorySpecs := aMetacelloPackageSpec repositorySpecs
		                          ifEmpty: [ currentRepositorySpecs ].
	directive := MetacelloDirective
		             loadPackage: aMetacelloPackageSpec
		             repositorySpecs: packageRepositorySpecs.
	(self specBuilder currentLoadDirective includes: directive) ifTrue: [
		^ self ].

	self preLoad: aMetacelloPackageSpec.
	directive addTo: self specBuilder currentLoadDirective.
	self postLoad: aMetacelloPackageSpec
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectReference: aMetacelloProjectReferenceSpec [

	| displayString projectSpec registered override registration requiredArray projectReferenceToLoad |
	projectReferenceToLoad := aMetacelloProjectReferenceSpec
		                          projectReference.

	displayString := 'Project: ' , projectReferenceToLoad name.
	projectReferenceToLoad versionString ifNotNil: [
		displayString := displayString , ' '
		                 , projectReferenceToLoad versionString ].

	MetacelloNotification signal: displayString.

	projectReferenceToLoad isLocked ifTrue: [
		MetacelloNotification signal: 'Project is locked, skipping.'.
		^ projectReferenceToLoad ].

	registration := MetacelloProjectRegistration
		                registrationForProjectSpec: projectReferenceToLoad
		                ifAbsent: [ :new | new ]
		                ifPresent: [ :existing :new |
		                engine resolvePresentProject: existing new: new ].
	registered := registration projectSpec.
	(registered compareEqual: projectReferenceToLoad) ifFalse: [ "counts as override, only if they differ in some aspect"
		override := registered copy.
		override mergeScriptLoads: projectReferenceToLoad.
		projectReferenceToLoad := override ].

	projectSpec := projectReferenceToLoad asProjectSpecForVersion:
		               projectReferenceToLoad versionOrNil.
	projectSpec ensureProjectLoadedWithEngine: engine.
	requiredArray := projectSpec loadListForVersion: projectSpec version.
	projectSpec version
		recordRequiredFromArray: requiredArray
		withEngine: engine.
	registration registerProject
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectSpec: aMetacelloMCBaselineOfProjectSpec [
	
	"Download the baseline!"
	| loadedSpec |
	loadedSpec := engine lookupProjectSpecFor: aMetacelloMCBaselineOfProjectSpec.

	"And interpret"
	currentRepositorySpecs := aMetacelloMCBaselineOfProjectSpec
		                          repositorySpecs.
	loadedSpec projectPackage acceptVisitor: self
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitVersionSpec: aMetacelloVersionSpec [

	self preLoad: aMetacelloVersionSpec.
	self visitVersionSpecChildren: aMetacelloVersionSpec.
	self postLoad: aMetacelloVersionSpec
]

{ #category : 'actions' }
MetacelloRecordTarget >> visitVersionSpecChildren: aSpec [

	| packages oldCurrentRepositorySpecs |
	
	packages := Dictionary new.
	aSpec resolveToLoadableSpecs: required map: packages.
	
	oldCurrentRepositorySpecs := currentRepositorySpecs.
	currentRepositorySpecs := aSpec repositorySpecs.
	packages := aSpec packageSpecsInLoadOrderForMap: packages.
	
	["Iterate the internals of the version spec"
	aSpec project loadType == #atomic ifTrue: [
		^ self atomicLoadPackages: packages ofSpec: aSpec ].
	aSpec project loadType == #linear ifTrue: [
		^ self linearLoadPackages: packages ofSpec: aSpec ].

	MetacelloError signal:
		'Unknown loadType: ' , aSpec project loadType printString]
		ensure: [ currentRepositorySpecs := oldCurrentRepositorySpecs ]
]
