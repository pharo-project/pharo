Class {
	#name : 'MetacelloRecordTarget',
	#superclass : 'MetacelloFetchTarget',
	#category : 'Metacello-Core-Loaders',
	#package : 'Metacello-Core',
	#tag : 'Loaders'
}

{ #category : 'accessing' }
MetacelloRecordTarget >> actionLabel [

	^'Recording '
]

{ #category : 'actions' }
MetacelloRecordTarget >> linearLoadPackageSpec: packageSpec gofer: gofer withEngine: anEngine [

	MetacelloPlatform current
		do: [
			| externalReference loadBlock answers |
			externalReference := self
				                     resolveRecordingPackageSpecReference:
				                     packageSpec
				                     gofer: gofer
				                     withEngine: anEngine.
			loadBlock := [
			             self preLoad: packageSpec withEngine: anEngine.
			             (MetacelloDirective
				              loadPackage: packageSpec
				              externalReference: externalReference) addTo:
				             anEngine specBuilder currentLoadDirective.
			             self postLoad: packageSpec withEngine: anEngine ].
			(answers := packageSpec answers) notEmpty
				ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
				ifFalse: [ "fake := packageSpec copy.
			fake name: fake file.
			anEngine loadData 
				addVersion: fake
				versionInfo: fake
				resolvedReference: externalReference 
				packageSpec: packageSpec"
					loadBlock value ] ]
		displaying: 'Recording ' , packageSpec file
]

{ #category : 'actions' }
MetacelloRecordTarget >> loadPackageDirectives: pkgLoads gofer: aGofer [
	"Noop"
]

{ #category : 'accessing' }
MetacelloRecordTarget >> packages [

	| packages |
	packages := OrderedCollection new.
	self loadDirective packageDirectivesDo: [:directive | packages add: directive spec ].
	^packages
]

{ #category : 'doits' }
MetacelloRecordTarget >> postLoad: packageOrVersionSpec withEngine: anEngine [

	^ self
]

{ #category : 'doits' }
MetacelloRecordTarget >> preLoad: packageOrVersionSpec withEngine: anEngine [

	^ self
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
