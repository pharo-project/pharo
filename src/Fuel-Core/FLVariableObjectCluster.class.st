"
I am a generic cluster for objects with indexable variables.
"
Class {
	#name : #FLVariableObjectCluster,
	#superclass : #FLPointerObjectCluster,
	#instVars : [
		'variableReferences'
	],
	#category : #'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLVariableObjectCluster >> add: anObject traceWith: aAnalysis [
	"Add an object to the cluster and trace references."
	objects addIfNotPresent: anObject ifPresentDo: [ ^ self ].

	self 
		referencesOf: anObject 
		do: [ :aChild || actual |
			actual := aChild fuelReplacement.
			self addReferenceFrom: anObject to: actual.
			aAnalysis trace: actual ].
		
	self
		variablePartReferencesOf: anObject
		do: [ :aChild || actual |
			actual := aChild fuelReplacement.
			self addVariableReferenceFrom: anObject to: actual.
			aAnalysis trace: actual ]
]

{ #category : #analyzing }
FLVariableObjectCluster >> addVariableReferenceFrom: anObject to: anotherObject [
	| list |
	list := self variableReferences
		at: anObject
		ifAbsent: [ nil ].
	list ifNil: [ self variableReferences at: anObject put: (list := OrderedCollection new) ].	
	list add: anotherObject
]

{ #category : #'serialize/materialize' }
FLVariableObjectCluster >> materializeInstanceWith: aDecoder [

	^theClass fuelNew: aDecoder nextEncodedPositiveInteger
]

{ #category : #'serialize/materialize' }
FLVariableObjectCluster >> materializeReferencesOf: anObject with: aDecoder [

	super materializeReferencesOf: anObject with: aDecoder.
	self materializeReferencesVariablePartOf: anObject with: aDecoder.
]

{ #category : #'serialize/materialize' }
FLVariableObjectCluster >> materializeReferencesVariablePartOf: anObject with: aDecoder [

	1 to: anObject basicSize 
		do: [ :index | anObject basicAt: index put: aDecoder nextEncodedReference ]
]

{ #category : #'serialize/materialize' }
FLVariableObjectCluster >> serializeInstance: anObject with: anEncoder [

	anEncoder encodePositiveInteger: anObject basicSize
]

{ #category : #'serialize/materialize' }
FLVariableObjectCluster >> serializeReferencesOf: anObject with: anEncoder [

	super serializeReferencesOf: anObject with: anEncoder.
	self serializeReferencesVariablePartOf: anObject with: anEncoder. 
]

{ #category : #'serialize/materialize' }
FLVariableObjectCluster >> serializeReferencesVariablePartOf: anObject with: anEncoder [
	
	(self variableReferences at: anObject ifAbsent: [ ^ self ]) do: [ :value |
		anEncoder encodeReferenceTo: value ]
]

{ #category : #analyzing }
FLVariableObjectCluster >> variablePartReferencesOf: anObject do: aBlock [ 

	1 to: anObject basicSize 
		do: [ :index | aBlock value: (anObject basicAt: index) ]
]

{ #category : #accessing }
FLVariableObjectCluster >> variableReferences [
	^ variableReferences ifNil: [ variableReferences := FLLargeIdentityDictionary new ]
]
