"
Can give information about a package that is saved in a repository, for example: 
- isLoaded if the package has been loaded into the image.
- isModified if the package has local changes to be commited
- incomingCommits information about commits in the repository that are newer than the version loaded into the image.

"
Class {
	#name : #IcePackage,
	#superclass : #Object,
	#instVars : [
		'package',
		'repository',
		'isDirty'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IcePackage class >> named: packageName repository: repository [
	^ self new 
		package: packageName;
		repository: repository; 
		yourself
]

{ #category : #comparing }
IcePackage >> = another [
	^ self species = another species 
		and: [ self repository = another repository 
		and: [ self name = another name ]]

]

{ #category : #loading }
IcePackage >> beClean [

	isDirty := false
]

{ #category : #loading }
IcePackage >> beDirty [

	isDirty := true
]

{ #category : #accessing }
IcePackage >> beDirty: aBoolean [
	
	isDirty := aBoolean
]

{ #category : #'accessing-monticello' }
IcePackage >> currentCommit [
	
	^ repository workingCopy referenceCommit
]

{ #category : #'accessing-monticello' }
IcePackage >> currentCommits [
	
	^ repository workingCopy referenceCommits
]

{ #category : #'*Iceberg-Adapters' }
IcePackage >> directoryName [

	^ self repository directoryNameForPackage: self
]

{ #category : #comparing }
IcePackage >> hash [
	^ self name hash
]

{ #category : #loading }
IcePackage >> initialize [
	super initialize.
	isDirty := false
]

{ #category : #loading }
IcePackage >> isClean [

	^ self isDirty not
]

{ #category : #loading }
IcePackage >> isDirty [

	^ self isLoaded and: [isDirty ifNil: [ isDirty := false ]]
]

{ #category : #testing }
IcePackage >> isExported [

	^ repository isPackageExported: self
]

{ #category : #testing }
IcePackage >> isLoaded [
	
	^ repository workingCopy isLoadedPackageNamed: self packageName
]

{ #category : #testing }
IcePackage >> isModified [
	
	^ self isDirty
]

{ #category : #TOMOVE }
IcePackage >> latestVersion [
	
	^ self versionFor: self repository head commit
]

{ #category : #loading }
IcePackage >> load [
	
	self repository workingCopy loadPackageNamed: self name
]

{ #category : #querying }
IcePackage >> loadedVersion [
	^ self repository workingCopy versionFor: self
]

{ #category : #'accessing-monticello' }
IcePackage >> mcPackage [
	
	^ MCPackage named: self packageName
]

{ #category : #'accessing-monticello' }
IcePackage >> mcSnapshotInRepository [
	"Answer a version of the package as it is present on repository. 
	 Useful to browse non-loaded packages."
	^ self repository branch snapshotFor: self
]

{ #category : #'accessing-monticello' }
IcePackage >> mcWorkingCopy [

	^ self isLoaded ifTrue: [
		self mcPackage workingCopy ]
]

{ #category : #accessing }
IcePackage >> name [
	^ package
]

{ #category : #querying }
IcePackage >> outgoingCommits [
	^ self repository branch outgoingCommits: [ :walk | walk modifyingPackage: self ]

]

{ #category : #accessing }
IcePackage >> package [
	^ package
]

{ #category : #accessing }
IcePackage >> package: anObject [
	package := anObject
]

{ #category : #accessing }
IcePackage >> packageName [
	^ package
]

{ #category : #printing }
IcePackage >> printDescriptionOn: aStream [
	aStream nextPut: $(;
		nextPutAll: package;
		nextPutAll: ', ';
		nextPutAll: (self isLoaded ifTrue: 'loaded' ifFalse: 'not loaded');
		nextPut: $)
]

{ #category : #printing }
IcePackage >> printOn: aStream [
	aStream nextPutAll: self class name.
	self printDescriptionOn: aStream
]

{ #category : #actions }
IcePackage >> reload [
	self load
]

{ #category : #actions }
IcePackage >> remove [
	"Removes the package from the repository.
	Does not unload from the image"
	repository workingCopy basicRemovePackage: self
]

{ #category : #accessing }
IcePackage >> repository [
	^ repository
]

{ #category : #accessing }
IcePackage >> repository: anObject [
	repository := anObject
]

{ #category : #actions }
IcePackage >> unload [

	repository workingCopy unloadPackage: self
]

{ #category : #querying }
IcePackage >> versionFor: aCommitId [
	^ aCommitId commit versionFor: self
]

{ #category : #versions }
IcePackage >> versionLike: selectBlock ifNone: ifNoneBlock [
	"Return all versions of this package in the current branch, latest first. 
	Containing info in the repo makes it easier to refresh."
	^ self repository branch 
		versionsFor: self 
		detect: selectBlock 
		ifNone: ifNoneBlock 

]

{ #category : #querying }
IcePackage >> versions [
	"Return all versions of this package in the current branch, latest first. 
	Containing info in the repo makes it easier to refresh."
	^ self repository branch versionsFor: self

]
