"
I hold contextual information relevant to the serialzation and materialization processes.

I make information available to any objects participating in the serialization or materialization process. I know the current configuration and hold a map of objects.

Users should use the TFLConfigurable trait and send #context to themselves to obtain my current instance.
"
Class {
	#name : #FLContext,
	#superclass : #Object,
	#traits : 'TFLConfigurable',
	#classTraits : 'TFLConfigurable classTrait',
	#instVars : [
		'map'
	],
	#category : #'Fuel-Core-Configuration'
}

{ #category : #accessing }
FLContext >> analysis [
	^ map at: #analysis
]

{ #category : #accessing }
FLContext >> analysis: anAnalysis [
	map
		at: #analysis
		put: anAnalysis
]

{ #category : #accessing }
FLContext >> finalizeOperations [
	self finalizers do: [ :finalizer |
		finalizer value ].
	
	self configuration shouldFinalizeStream ifFalse: [ ^ self ].
	
	self configuration streamFinalizer value: self stream
]

{ #category : #accessing }
FLContext >> finalizers [
	^ map
		at: #finalizers
		ifAbsent: [ #() ]
]

{ #category : #'exception-handling' }
FLContext >> handleObjectLimitExceeded [
	map
		at: #objectLimitExceededWarningRaised
		ifPresent: [ :boolean | ^ self ]
		ifAbsentPut: [ true].
		
	self configuration isLimitObjectsExceptionEnabled ifFalse: [ ^ self ].

	FLObjectLimitExceededWarning signal
]

{ #category : #accessing }
FLContext >> initialize [
	super initialize.
	
	map := IdentityDictionary new
]

{ #category : #accessing }
FLContext >> materializedObjects [
	^ map
		at: #materializedObjects
		ifAbsentPut: [ FLMaterializedObjects new ]
]

{ #category : #accessing }
FLContext >> object [
	^ map
		at: #object
		ifAbsentPut: [ self configuration object ]
]

{ #category : #accessing }
FLContext >> object: anObject [
	map
		at: #object
		put: anObject
]

{ #category : #accessing }
FLContext >> registerFinalizer: aBlock [
	(map
		at: #finalizers
		ifAbsentPut: [ OrderedCollection new ])
			add: aBlock
]

{ #category : #accessing }
FLContext >> stream [
	^ map
		at: #stream
		ifAbsentPut: [ self configuration streamFactory value ]
]

{ #category : #accessing }
FLContext >> stream: aStream [
	^ map
		at: #stream
		put: aStream
]

{ #category : #evaluating }
FLContext >> useDuring: aBlock [
	^ FLCurrentContext
		value: self
		during: aBlock
]
