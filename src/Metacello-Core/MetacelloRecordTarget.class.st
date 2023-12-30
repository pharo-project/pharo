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

	| directive |
	directive := MetacelloDirective
		             loadPackage: aMetacelloPackageSpec
		             repositorySpecs: currentRepositorySpecs.
	(self specBuilder currentLoadDirective includes: directive) ifTrue: [
		^ self ].

	self preLoad: aMetacelloPackageSpec.
	directive addTo: self specBuilder currentLoadDirective.
	self postLoad: aMetacelloPackageSpec
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectReference: aMetacelloProjectReferenceSpec [

	| displayString vrsn projectSpecForVersion registered override registration |
	registration := MetacelloProjectRegistration
		                registrationForProjectSpec:
		                aMetacelloProjectReferenceSpec projectReference
		                ifAbsent: [ :new | new ]
		                ifPresent: [ :existing :new |
		                engine resolvePresentProject: existing new: new ].
	registered := registration projectSpec.
	(registered compareEqual:
		 aMetacelloProjectReferenceSpec projectReference) ifFalse: [ "counts as override, only if they differ in some aspect"
		override := registered ].

	displayString := 'Project: '
	                 ,
	                 aMetacelloProjectReferenceSpec projectReference name.
	(aMetacelloProjectReferenceSpec projectReference versionStringForEngine: engine) ifNotNil: [
		displayString := displayString , ' '
		                 , (aMetacelloProjectReferenceSpec projectReference
				                  versionStringForEngine: engine) ].

	MetacelloNotification signal: displayString.

	aMetacelloProjectReferenceSpec projectReference isLocked ifTrue: [
		MetacelloNotification signal: 'Project is locked, skipping.'.
		^ aMetacelloProjectReferenceSpec projectReference ].


	override ifNotNil: [
		override := override copy.
		override mergeScriptLoads:
			aMetacelloProjectReferenceSpec projectReference.
		override loadVersion: nil.
		^ self ].

	vrsn := aMetacelloProjectReferenceSpec projectReference
		        versionOrNilForEngine: engine.
	projectSpecForVersion := aMetacelloProjectReferenceSpec
		                         projectReference asProjectSpecForVersion:
		                         vrsn.

	projectSpecForVersion ensureProjectLoadedWithEngine: engine.
	vrsn ifNil: [ vrsn := projectSpecForVersion versionForEngine: engine ].
	vrsn
		recordRequiredFromArray:
		(projectSpecForVersion loadListForVersion: vrsn)
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
