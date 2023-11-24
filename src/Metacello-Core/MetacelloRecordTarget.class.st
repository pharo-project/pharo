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
MetacelloRecordTarget >> atomicLoadPackageOfSpec: aSpec withEngine: anEngine [

	anEngine specBuilder
		pushAtomicLoadDirectivesDuring: [
			super linearLoadPackagesOfSpec: aSpec withEngine: anEngine ]
		for: self
]

{ #category : 'actions' }
MetacelloRecordTarget >> linearLoadPackageSpec: packageSpec gofer: gofer withEngine: anEngine [

	| externalReference |
	externalReference := self
		                     resolveRecordingPackageSpecReference:
		                     packageSpec
		                     gofer: gofer
		                     withEngine: anEngine.
	self preLoad: packageSpec withEngine: anEngine.
	(MetacelloDirective
		 loadPackage: packageSpec
		 externalReference: externalReference) addTo:
		anEngine specBuilder currentLoadDirective.
	self postLoad: packageSpec withEngine: anEngine
]

{ #category : 'private' }
MetacelloRecordTarget >> linearLoadPackagesOfSpec: aSpec withEngine: anEngine [

	anEngine specBuilder
		pushLinearLoadDirectivesDuring: [
			super linearLoadPackagesOfSpec: aSpec withEngine: anEngine ]
		for: self
]

{ #category : 'actions' }
MetacelloRecordTarget >> load: aSpec withEngine: anEngine [

	aSpec project loadType == #atomic ifTrue: [
		^ self atomicLoadPackageOfSpec: aSpec withEngine: anEngine ].
	aSpec project loadType == #linear ifTrue: [
		^ self linearLoadPackagesOfSpec: aSpec withEngine: anEngine ].

	MetacelloError signal:
		'Unknown loadType: ' , aSpec project loadType printString
]

{ #category : 'actions' }
MetacelloRecordTarget >> loadPackageDirectives: pkgLoads gofer: aGofer [


	"Noop"
]

{ #category : 'doits' }
MetacelloRecordTarget >> postLoad: packageOrVersionSpec withEngine: anEngine [

	(MetacelloDirective postLoadSpec: packageOrVersionSpec) addTo:
		anEngine specBuilder currentLoadDirective
]

{ #category : 'doits' }
MetacelloRecordTarget >> preLoad: packageOrVersionSpec withEngine: anEngine [

	(MetacelloDirective preLoadSpec: packageOrVersionSpec) addTo:
		anEngine specBuilder currentLoadDirective
]

{ #category : 'private' }
MetacelloRecordTarget >> resolveRecordingPackageSpecReference: packageSpec gofer: gofer withEngine: anEngine [

	| externalPackageReference |
	externalPackageReference := packageSpec file
		                            ifNil: [
		                            GoferPackageReference name:
			                            packageSpec name ]
		                            ifNotNil: [ :file |
		                            GoferResolvedReference
			                            name: file
			                            repository: nil ].
	anEngine repositoryMap
		at: externalPackageReference packageName
		put: (packageSpec repositorySpecs collect: [ :repoSpec |
				 repoSpec createRepository ]).
	^ externalPackageReference
]
