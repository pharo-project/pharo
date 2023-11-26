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
MetacelloRecordTarget >> atomicLoadPackageOfSpec: aSpec [

	self specBuilder
		pushAtomicLoadDirectivesDuring: [
			currentRepositorySpecs := aSpec repositorySpecs.
			aSpec packageSpecsInLoadOrder do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'accessing' }
MetacelloRecordTarget >> currentRepositorySpecs: aCollection [ 
	currentRepositorySpecs := aCollection
]

{ #category : 'actions' }
MetacelloRecordTarget >> linearLoadPackageSpec: packageSpec gofer: gofer [

	| externalReference |
	externalReference := self
		                     resolveRecordingPackageSpecReference:
		                     packageSpec
		                     gofer: gofer.

	"Exit if already loaded"
	externalReference ifNil: [ ^ nil ].
	self preLoad: packageSpec.
	(MetacelloDirective
		 loadPackage: packageSpec
		 externalReference: externalReference) addTo:
		self specBuilder currentLoadDirective.
	self postLoad: packageSpec.

	^ externalReference
]

{ #category : 'private' }
MetacelloRecordTarget >> linearLoadPackagesOfSpec: aSpec [

	self specBuilder
		pushLinearLoadDirectivesDuring: [
			currentRepositorySpecs := aSpec repositorySpecs.
			aSpec packageSpecsInLoadOrder do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'actions' }
MetacelloRecordTarget >> loadPackageDirectives: pkgLoads gofer: aGofer [


	"Noop"
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
MetacelloRecordTarget >> resolveRecordingPackageSpecReference: packageSpec gofer: gofer [

	^ (self resolvePackageSpecReferences: packageSpec gofer: gofer) first
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

	| gofer monticelloRepositories externalReference |
	gofer := Gofer new.
	monticelloRepositories := self repositoriesFrom:
		                          currentRepositorySpecs.
	monticelloRepositories do: [ :repo | gofer repository: repo ].


	externalReference := self
		                     resolveRecordingPackageSpecReference:
		                     aMetacelloPackageSpec
		                     gofer: gofer.

	"Exit if already loaded"
	externalReference ifNil: [ ^ nil ].
	self preLoad: aMetacelloPackageSpec.
	(MetacelloDirective
		 loadPackage: aMetacelloPackageSpec
		 externalReference: externalReference) addTo:
		self specBuilder currentLoadDirective.
	self postLoad: aMetacelloPackageSpec.

	^ externalReference
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectReference: aMetacelloProjectReferenceSpec [

	^ (MetacelloLookupProjectSpecForLoad new
		   projectSpec: aMetacelloProjectReferenceSpec projectReference;
		   yourself) signal performLoadWithEngine: engine usingTarget: self
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectSpec: aMetacelloMCBaselineOfProjectSpec [

	currentRepositorySpecs := aMetacelloMCBaselineOfProjectSpec
		                          repositorySpecs.
	aMetacelloMCBaselineOfProjectSpec projectPackage acceptVisitor: self
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitVersionSpec: aMetacelloVersionSpec [

	| packages |
	packages := Dictionary new.
	aMetacelloVersionSpec resolveToLoadableSpecs: required map: packages.

	packages valuesDo: [ :pkg |
		pkg ensureLoadedForDevelopmentUsing: self withEngine: engine.
		(MetacelloIgnorePackageLoaded signal: pkg) ifFalse: [
			engine ignoreImage ifFalse: [
				(pkg
					 compareCurrentVersion: self operator
					 targetVersionStatus: #( #allLoadedToSpec )
					 using: self
					 withEngine: engine) ifTrue: [ packages removeKey: pkg name ] ] ] ].

	packages ifEmpty: [ ^ self ].
	self preLoad: aMetacelloVersionSpec.
	self visitVersionSpecChildren: aMetacelloVersionSpec.
	self postLoad: aMetacelloVersionSpec
]

{ #category : 'actions' }
MetacelloRecordTarget >> visitVersionSpecChildren: aSpec [

	"Iterate the internals of the version spec"
	aSpec project loadType == #atomic ifTrue: [
		^ self atomicLoadPackageOfSpec: aSpec ].
	aSpec project loadType == #linear ifTrue: [
		^ self linearLoadPackagesOfSpec: aSpec ].

	MetacelloError signal:
		'Unknown loadType: ' , aSpec project loadType printString
]
