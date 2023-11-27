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
MetacelloRecordTarget >> atomicLoadPackageOfSpec: packages [

	self specBuilder
		pushAtomicLoadDirectivesDuring: [
			packages do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'accessing' }
MetacelloRecordTarget >> currentRepositorySpecs: aCollection [ 
	currentRepositorySpecs := aCollection
]

{ #category : 'private' }
MetacelloRecordTarget >> linearLoadPackagesOfSpec: packages [

	self specBuilder
		pushLinearLoadDirectivesDuring: [
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

	self preLoad: aMetacelloPackageSpec.
	(MetacelloDirective
		 loadPackage: aMetacelloPackageSpec
		 externalReference: currentRepositorySpecs) addTo:
		self specBuilder currentLoadDirective.
	self postLoad: aMetacelloPackageSpec
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectReference: aMetacelloProjectReferenceSpec [

	| specForLoad displayString vrsn |
	specForLoad := engine handleLookupProjectSpecForLoad:
		               aMetacelloProjectReferenceSpec projectReference.

	displayString := 'Project: ' , specForLoad projectSpec name.
	specForLoad projectSpec versionString ifNotNil: [
		displayString := displayString , ' '
		                 , specForLoad projectSpec versionString ].

	MetacelloNotification signal: displayString.

	specForLoad projectSpec isLocked ifTrue: [
		MetacelloNotification signal: 'Project is locked, skipping.'.
		^ specForLoad ].


	specForLoad hasOverride ifTrue: [
		| override |
		override := specForLoad overrideProjectSpec copy.
		override mergeScriptLoads: specForLoad projectSpec.
		override loadVersion: nil.
		^ self ].

	vrsn := specForLoad projectSpec versionOrNil.
	(specForLoad projectSpec asProjectSpecForVersion: vrsn)
		loadVersion: vrsn
		usingTarget: self
		withEngine: engine
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

	| packages |
	
	packages := Dictionary new.
	aSpec resolveToLoadableSpecs: required map: packages.
	currentRepositorySpecs := aSpec repositorySpecs.
	packages := aSpec packageSpecsInLoadOrderForMap: packages.
	
	"Iterate the internals of the version spec"
	aSpec project loadType == #atomic ifTrue: [
		^ self atomicLoadPackageOfSpec: packages ].
	aSpec project loadType == #linear ifTrue: [
		^ self linearLoadPackagesOfSpec: packages ].

	MetacelloError signal:
		'Unknown loadType: ' , aSpec project loadType printString
]
