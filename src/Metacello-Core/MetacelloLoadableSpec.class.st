Class {
	#name : 'MetacelloLoadableSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'includes'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'class initialization' }
MetacelloLoadableSpec class >> initialize [
	"To remove later. Added in Pharo 14"

	self deprecatedAliases: #( MetacelloAbstractPackageSpec )
]

{ #category : 'testing' }
MetacelloLoadableSpec class >> isAbstract [

	^ self = MetacelloLoadableSpec
]

{ #category : 'querying' }
MetacelloLoadableSpec >> includes [

	^ includes ifNil: [ includes := #(  ) ]
]

{ #category : 'accessing' }
MetacelloLoadableSpec >> includes: aCollection [

	includes := aCollection isString
		            ifTrue: [ { aCollection } ]
		            ifFalse: [ aCollection asArray ]
]

{ #category : 'private' }
MetacelloLoadableSpec >> includesForPackageOrdering [

	^ #(  )
]

{ #category : 'testing' }
MetacelloLoadableSpec >> isGroupSpec [

	^ false
]

{ #category : 'testing' }
MetacelloLoadableSpec >> isPackageSpec [
	 ^ false
]

{ #category : 'testing' }
MetacelloLoadableSpec >> isProjectReferenceSpec [
	 ^ false
]

{ #category : 'printing' }
MetacelloLoadableSpec >> label [

	^self name
]

{ #category : 'merging' }
MetacelloLoadableSpec >> mergeSpec: anotherSpec [

	super mergeSpec: anotherSpec.
	anotherSpec includes ifNotEmpty: [ :anotherIncludes | self includes: self includes , anotherIncludes ]
]

{ #category : 'querying' }
MetacelloLoadableSpec >> name [

	^ self subclassResponsibility
]

{ #category : 'copying' }
MetacelloLoadableSpec >> postCopy [

	super postCopy.
	includes := includes copy
]

{ #category : 'printing' }
MetacelloLoadableSpec >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' [';
		nextPutAll: self name;
		nextPutAll: ']'
]

{ #category : 'accessing' }
MetacelloLoadableSpec >> referencedSpec [

	^self
]

{ #category : 'loading' }
MetacelloLoadableSpec >> repositorySpecs [

	^#()
]

{ #category : 'private' }
MetacelloLoadableSpec >> requiredSpecNamesForPackageOrdering: aVersionSpec [

	^ #(  )
]

{ #category : 'testing' }
MetacelloLoadableSpec >> requires [

	^ #(  )
]

{ #category : 'private' }
MetacelloLoadableSpec >> resolveSpecsForPackageOrdering: aVersionSpec [

	^ { self }
]

{ #category : 'private' }
MetacelloLoadableSpec >> resolveToLoadableSpec [

	^self
]

{ #category : 'private' }
MetacelloLoadableSpec >> resolveToPackagesAndProjectsIn: aVersionSpec [

	^ self subclassResponsibility
]

{ #category : 'querying' }
MetacelloLoadableSpec >> version [ 
	"MetacelloPackageSpec compatibility"
	
	^nil
]
