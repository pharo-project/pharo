Class {
	#name : 'MetacelloRecordTarget',
	#superclass : 'MetacelloTarget',
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
		super linearLoadPackagesOfSpec: aSpec ]
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
		super linearLoadPackagesOfSpec: aSpec ]
		for: self
]

{ #category : 'actions' }
MetacelloRecordTarget >> load: aSpec [

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

{ #category : 'private' }
MetacelloRecordTarget >> resolveRecordingPackageSpecReference: packageSpec gofer: gofer [

	^ (self resolvePackageSpecReferences: packageSpec gofer: gofer) first
]
