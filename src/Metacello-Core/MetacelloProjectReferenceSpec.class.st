Class {
	#name : 'MetacelloProjectReferenceSpec',
	#superclass : 'MetacelloLoadableSpec',
	#instVars : [
		'projectReference'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'accessing' }
MetacelloProjectReferenceSpec class >> projectReference: aMetacelloProjectSpec [

	^ self new
		  projectReference: aMetacelloProjectSpec;
		  yourself
]

{ #category : 'visiting' }
MetacelloProjectReferenceSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitProjectReference: self
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> includes: aCollection [

	self shouldNotImplement
]

{ #category : 'testing' }
MetacelloProjectReferenceSpec >> isProjectReferenceSpec [

	^ true
]

{ #category : 'importing' }
MetacelloProjectReferenceSpec >> mergeImportLoads: aLoadList [
    self projectReference mergeImportLoads: aLoadList
]

{ #category : 'merging' }
MetacelloProjectReferenceSpec >> mergeSpec: anotherSpec [

	super mergeSpec: anotherSpec.
	self projectReference className. "Before merging the reference, we need to launch the defensive strategy of the class name."
	self projectReference: (self projectReference mergeSpec: anotherSpec projectReference)
]

{ #category : 'querying' }
MetacelloProjectReferenceSpec >> name [

	^ self projectReference name
]

{ #category : 'loading' }
MetacelloProjectReferenceSpec >> packageSpecsInLoadOrder [

	| specs loadedSpecs loads |
	specs := self projectReference version packageSpecsInLoadOrder.
	loads := self projectReference loadListForVersion: self projectReference version.
	loadedSpecs := specs select: [ :each | loads includes: each name ].
	^ loadedSpecs flatCollect: [ :e | e packageSpecsInLoadOrder ]
]

{ #category : 'copying' }
MetacelloProjectReferenceSpec >> postCopy [

	super postCopy.
	projectReference := projectReference copy.
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> projectLabel [
    ^ self projectReference projectLabel
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> projectName [
	^self projectReference name
]

{ #category : 'querying' }
MetacelloProjectReferenceSpec >> projectReference [
	^ projectReference
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> projectReference: anObject [
	projectReference := anObject
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> referencedSpec [

	^self projectReference
]

{ #category : 'querying' }
MetacelloProjectReferenceSpec >> repositories [
    ^ self referencedSpec repositories
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> repository: aStringOrMetacelloRepositorySpec [
    ^ self projectReference repository: aStringOrMetacelloRepositorySpec
]

{ #category : 'loading' }
MetacelloProjectReferenceSpec >> repositorySpecs [

	^ self repositories repositorySpecs
]

{ #category : 'private' }
MetacelloProjectReferenceSpec >> resolveToLoadableSpec [

	^self projectReference resolveToLoadableSpec
]

{ #category : 'private' }
MetacelloProjectReferenceSpec >> resolveToPackagesAndProjectsIn: aVersionSpec [

	^ { self }
]

{ #category : 'querying' }
MetacelloProjectReferenceSpec >> version [

	^self projectReference version
]
