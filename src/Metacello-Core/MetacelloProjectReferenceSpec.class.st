Class {
	#name : 'MetacelloProjectReferenceSpec',
	#superclass : 'MetacelloAbstractPackageSpec',
	#instVars : [
		'projectReference'
	],
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'merging' }
MetacelloProjectReferenceSpec >> aboutToCopy [
    self projectReference className
]

{ #category : 'visiting' }
MetacelloProjectReferenceSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitProjectReference: self
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> answers: aListOfPairs [

	self shouldNotImplement
]

{ #category : 'printing' }
MetacelloProjectReferenceSpec >> configMethodCascadeOn: aStream member: aMember last: lastCascade indent: indent [

	aMember methodUpdateSelector == #remove:
		ifTrue: [ aStream nextPutAll: 'removeProject: ' , self name printString ]
		ifFalse: [
			self projectReference ifNil: [ ^ self ].
			aStream nextPutAll: self projectLabel , ': ' , self projectName printString , ' '.
			(aMember methodUpdateSelector == #copy: and: [ self projectReference hasNonVersionStringField ]) ifTrue: [
				aStream nextPutAll: 'copyFrom: ' , aMember sourceName printString , ' ' ].
			self projectReference configShortCutMethodOn: aStream member: aMember indent: indent + 1 ].
	lastCascade
		ifTrue: [ aStream nextPut: $. ]
		ifFalse: [
			aStream
				nextPut: $;;
				cr ]
]

{ #category : 'printing' }
MetacelloProjectReferenceSpec >> configMethodOn: aStream indent: indent [

	aStream 
		tab: indent; nextPutAll: 'spec '; cr;
		tab: indent + 1; nextPutAll: 'name: ', self name printString; nextPut: $;; cr;
		tab: indent + 1; nextPutAll: 'projectReference: '; nextPut: $[; cr.
	aStream 
		tab: indent + 2; nextPutAll: 'spec'.
	self projectReference ifNotNil: [ self projectReference configMethodBodyOn: aStream indent: indent + 2].
	aStream nextPutAll: ' ].'
]

{ #category : 'testing' }
MetacelloProjectReferenceSpec >> hasRepository [
    ^ self projectReference hasRepository
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
MetacelloProjectReferenceSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #projectReference put: projectReference.
	^map
]

{ #category : 'merging' }
MetacelloProjectReferenceSpec >> mergeSpec: anotherSpec [

	| newSpec map |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	(map at: #projectReference) ifNotNil: [ :anotherProjectReference |
		newSpec projectReference: (newSpec projectReference
				 ifNotNil: [ :reference | reference mergeSpec: anotherProjectReference ]
				 ifNil: [ anotherProjectReference ]) ].
	^ newSpec
]

{ #category : 'merging' }
MetacelloProjectReferenceSpec >> nonOverridable [

	^super nonOverridable, #( projectReference )
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

{ #category : 'loading' }
MetacelloProjectReferenceSpec >> packageSpecsInLoadOrderWithEngine: anEngine [

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

	^self repositories map values
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> requires: aCollection [

	self shouldNotImplement
]

{ #category : 'private' }
MetacelloProjectReferenceSpec >> resolveToLoadableSpec [

	^self projectReference resolveToLoadableSpec
]

{ #category : 'private' }
MetacelloProjectReferenceSpec >> resolveToPackagesAndProjectsIn: aVersionSpec visited: visited [

	^ { self }
]

{ #category : 'private' }
MetacelloProjectReferenceSpec >> resolveToPackagesIn: aVersionSpec visited: visited [

	^#()
]

{ #category : 'querying' }
MetacelloProjectReferenceSpec >> version [

	^self projectReference version
]

{ #category : 'querying' }
MetacelloProjectReferenceSpec >> versionOrNil [

	^ self projectReference versionOrNil
]

{ #category : 'accessing' }
MetacelloProjectReferenceSpec >> versionString [

	^self projectReference versionString
]

{ #category : 'querying' }
MetacelloProjectReferenceSpec >> versionString: aString [

	^self projectReference versionString: aString
]
