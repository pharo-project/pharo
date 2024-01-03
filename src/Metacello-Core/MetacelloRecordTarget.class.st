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

	self preLoad: aMetacelloPackageSpec.
	(MetacelloDirective
		 loadPackage: aMetacelloPackageSpec
		 externalReference: currentRepositorySpecs) addTo:
		self specBuilder currentLoadDirective.
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
	aMetacelloProjectReferenceSpec projectReference versionString
		ifNotNil: [
			displayString := displayString , ' '
			                 ,
			                 aMetacelloProjectReferenceSpec projectReference
				                 versionString ].

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

	vrsn := aMetacelloProjectReferenceSpec projectReference versionOrNil.
	projectSpecForVersion := aMetacelloProjectReferenceSpec
		                         projectReference asProjectSpecForVersion:
		                         vrsn.
	projectSpecForVersion ensureProjectLoadedWithEngine: engine.
	vrsn ifNil: [ vrsn := projectSpecForVersion version ].
	vrsn
		recordRequiredFromArray:
		(projectSpecForVersion loadListForVersion: vrsn)
		withEngine: engine.
	registration registerProject.
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

	| packages oldCurrentRepositorySpecs |
	packages := Dictionary new.
	aMetacelloVersionSpec resolveToLoadableSpecs: required map: packages.
	oldCurrentRepositorySpecs := currentRepositorySpecs.
	currentRepositorySpecs := aMetacelloVersionSpec repositorySpecs.
	packages := aMetacelloVersionSpec packageSpecsInLoadOrderForMap:
		            packages.
	self preLoad: aMetacelloVersionSpec.

	[ "Iterate the internals of the version spec"
	aMetacelloVersionSpec project loadType == #atomic ifTrue: [
		self atomicLoadPackages: packages ofSpec: aMetacelloVersionSpec ].
	aMetacelloVersionSpec project loadType == #linear ifTrue: [
		self linearLoadPackages: packages ofSpec: aMetacelloVersionSpec ].

	self postLoad: aMetacelloVersionSpec.

	(#( atomic linear ) includes: aMetacelloVersionSpec project loadType)
		ifFalse: [
			MetacelloError signal: 'Unknown loadType: '
				, aMetacelloVersionSpec project loadType printString ]
			
	] ensure: [ currentRepositorySpecs := oldCurrentRepositorySpecs ]
]
