Class {
	#name : 'MetacelloRecordTarget',
	#superclass : 'MetacelloTarget',
	#instVars : [
		'required',
		'currentRepositorySpecs'
	],
	#category : 'Metacello-Core-Loaders',
	#package : 'Metacello-Core',
	#tag : 'Loaders'
}

{ #category : 'accessing' }
MetacelloRecordTarget >> actionLabel [

	^'Recording '
]

{ #category : 'private' }
MetacelloRecordTarget >> atomicLoadPackageOfSpec: aSpec [

	engine specBuilder
		pushAtomicLoadDirectivesDuring: [
			currentRepositorySpecs := aSpec repositorySpecs.
			aSpec packageSpecsInLoadOrder do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
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
		engine specBuilder currentLoadDirective.
	self postLoad: packageSpec.

	^ externalReference
]

{ #category : 'private' }
MetacelloRecordTarget >> linearLoadPackagesOfSpec: aSpec [

	engine specBuilder
		pushLinearLoadDirectivesDuring: [
			currentRepositorySpecs := aSpec repositorySpecs.
			aSpec packageSpecsInLoadOrder do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'actions' }
MetacelloRecordTarget >> load: aSpec [

1halt.
	aSpec project loadType == #atomic ifTrue: [
		^ self atomicLoadPackageOfSpec: aSpec ].
	aSpec project loadType == #linear ifTrue: [
		^ self linearLoadPackagesOfSpec: aSpec ].

	MetacelloError signal:
		'Unknown loadType: ' , aSpec project loadType printString
]

{ #category : 'actions' }
MetacelloRecordTarget >> loadPackageDirectives: pkgLoads gofer: aGofer [


	"Noop"
]

{ #category : 'doits' }
MetacelloRecordTarget >> postLoad: packageOrVersionSpec [

	(MetacelloDirective postLoadSpec: packageOrVersionSpec) addTo:
		engine specBuilder currentLoadDirective
]

{ #category : 'doits' }
MetacelloRecordTarget >> preLoad: packageOrVersionSpec [

	(MetacelloDirective preLoadSpec: packageOrVersionSpec) addTo:
		engine specBuilder currentLoadDirective
]

{ #category : 'accessing' }
MetacelloRecordTarget >> required: aCollection [ 
	
	required := aCollection
]

{ #category : 'private' }
MetacelloRecordTarget >> resolveRecordingPackageSpecReference: packageSpec gofer: gofer [

	^ (self resolvePackageSpecReferences: packageSpec gofer: gofer) first
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
		engine specBuilder currentLoadDirective.
	self postLoad: aMetacelloPackageSpec.

	^ externalReference
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectReference: aMetacelloProjectReferenceSpec [

	| required |
	required := aMetacelloProjectReferenceSpec resolveToLoadableSpec.
	^ required loadUsingTarget: self withEngine: engine
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitVersionSpec: aMetacelloVersionSpec [

	| packages |
	packages := Dictionary new.
	aMetacelloVersionSpec resolveToLoadableSpecs: required map: packages.

	packages values do: [ :pkg |
		pkg ensureLoadedForDevelopmentUsing: self withEngine: engine.
		(MetacelloIgnorePackageLoaded signal: pkg) ifFalse: [
			engine ignoreImage ifFalse: [
				(pkg
					 compareCurrentVersion: self operator
					 targetVersionStatus: #( #allLoadedToSpec )
					 using: self
					 withEngine: engine) ifTrue: [ packages removeKey: pkg name ] ] ] ].

	packages notEmpty ifTrue: [
		self preLoad: aMetacelloVersionSpec.
		self load: aMetacelloVersionSpec.
		self postLoad: aMetacelloVersionSpec ]
]
