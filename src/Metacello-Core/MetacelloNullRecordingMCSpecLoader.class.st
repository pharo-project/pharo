Class {
	#name : 'MetacelloNullRecordingMCSpecLoader',
	#superclass : 'MetacelloFetchingMCSpecLoader',
	#category : 'Metacello-Core-Loaders',
	#package : 'Metacello-Core',
	#tag : 'Loaders'
}

{ #category : 'accessing' }
MetacelloNullRecordingMCSpecLoader >> actionLabel [

	^'Recording '
]

{ #category : 'versionInfo' }
MetacelloNullRecordingMCSpecLoader >> currentVersionInfoFor: packageSpec [

	^self loadData
		currentVersionInfoFor: packageSpec 
		ifAbsent: [ nil ]
]

{ #category : 'actions' }
MetacelloNullRecordingMCSpecLoader >> doingLoads: aBlock [
	"escape mechanism for recording and null loaders to skip doing loaderlike things"
]

{ #category : 'testing' }
MetacelloNullRecordingMCSpecLoader >> ensureForDevelopment [

	^false
]

{ #category : 'actions' }
MetacelloNullRecordingMCSpecLoader >> linearLoadPackageSpec: packageSpec gofer: gofer [

	MetacelloPlatform current
		do:  [ | externalReference loadBlock answers fake |
			externalReference := self resolveRecordingPackageSpecReference: packageSpec gofer: gofer.
			loadBlock := [
					self preLoad: packageSpec.
					(MetacelloDirective 
						loadPackage: packageSpec 
						externalReference: externalReference 
						loader: self) addTo: self loadDirective.
					self postLoad: packageSpec ].
			(answers := packageSpec answers) notEmpty
				ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
				ifFalse: [ loadBlock value ].
			fake := packageSpec copy.
			fake name: fake file.
			self loadData 
				addVersion: fake
				versionInfo: fake
				resolvedReference: externalReference 
				packageSpec: packageSpec ]
		displaying: 'Recording ', packageSpec file
]

{ #category : 'actions' }
MetacelloNullRecordingMCSpecLoader >> loadPackageDirective: aPackageLoadDirective gofer: aGofer [
	"Noop"
]

{ #category : 'actions' }
MetacelloNullRecordingMCSpecLoader >> loadPackageDirectives: pkgLoads gofer: aGofer [
	"Noop"
]

{ #category : 'accessing' }
MetacelloNullRecordingMCSpecLoader >> loadingSpecLoader [

	^self
]

{ #category : 'accessing' }
MetacelloNullRecordingMCSpecLoader >> packages [

	| packages |
	packages := OrderedCollection new.
	self loadDirective packageDirectivesDo: [:directive | packages add: directive spec ].
	^packages
]

{ #category : 'doits' }
MetacelloNullRecordingMCSpecLoader >> postLoad: packageOrVersionSpec [

	^ self
]

{ #category : 'doits' }
MetacelloNullRecordingMCSpecLoader >> preLoad: packageOrVersionSpec [

	^ self
]

{ #category : 'accessing' }
MetacelloNullRecordingMCSpecLoader >> recordingSpecLoader [

	^self
]

{ #category : 'private' }
MetacelloNullRecordingMCSpecLoader >> resolveRecordingPackageSpecReference: packageSpec gofer: gofer [
  | externalPackageReference |
  externalPackageReference := packageSpec file
	ifNil: [ GoferPackageReference name: packageSpec name ]
    ifNotNil: [ :file | GoferResolvedReference name: file repository: nil ].
  packageSpec repositorySpecs isEmpty
    ifTrue: [ 
      self repositoryMap
        at: externalPackageReference packageName
        put: (gofer repositories reject: [ :repo | repo = MCCacheRepository default ]) ]
    ifFalse: [ 
      self repositoryMap
        at: externalPackageReference packageName
        put: (packageSpec repositorySpecs collect: [ :repoSpec | repoSpec createRepository ]) ].
  ^ externalPackageReference
]
