Class {
	#name : 'MetacelloProjectReferenceSpec',
	#superclass : 'MetacelloAbstractPackageSpec',
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

{ #category : 'merging' }
MetacelloProjectReferenceSpec >> aboutToCopy [
    self projectReference className
]

{ #category : 'visiting' }
MetacelloProjectReferenceSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitProjectReference: self
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> includes: aCollection [

	self shouldNotImplement
]

{ #category : 'importing' }
MetacelloProjectReferenceSpec >> mergeImportLoads: aLoadList [
    self projectReference mergeImportLoads: aLoadList
]

{ #category : 'merging' }
MetacelloProjectReferenceSpec >> mergeSpec: anotherSpec [

	| newSpec |
	newSpec := super mergeSpec: anotherSpec.
	anotherSpec projectReference ifNotNil: [ :anotherProjectReference |
			newSpec projectReference: (newSpec projectReference
					 ifNotNil: [ :reference | reference mergeSpec: anotherProjectReference ]
					 ifNil: [ anotherProjectReference ]) ].
	^ newSpec
]

{ #category : 'querying' }
MetacelloProjectReferenceSpec >> name [

	^ self projectReference name
]

{ #category : 'loading' }
MetacelloProjectReferenceSpec >> packageSpecsInLoadOrder [

	| specs loadedSpecs loads |
	specs := self projectReference version spec packageSpecsInLoadOrder.
	loads := self projectReference loadListForVersion:
		         self projectReference version.
	loadedSpecs := specs select: [ :each | loads includes: each name ].
	^ loadedSpecs flatCollect: [ :e | e packageSpecsInLoadOrder ]
]

{ #category : 'copying' }
MetacelloProjectReferenceSpec >> postCopy [

	super postCopy.
	projectReference := projectReference copy.
]

{ #category : 'visiting' }
MetacelloProjectReferenceSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	projectBlock value: self
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
