"
I am a specialization of FLSerializer that knows how to serializer the header.

I copy the current configuration and manipulated some values to make the configuration works for header serialization. I also prevent recursive header serialization.
"
Class {
	#name : 'FLHeaderSerializer',
	#superclass : 'FLSerializer',
	#category : 'Fuel-Core-Configuration',
	#package : 'Fuel-Core',
	#tag : 'Configuration'
}

{ #category : 'initialization' }
FLHeaderSerializer >> initialize [
	super initialize.
	
	"Copy the current configuration"
	self copyFrom: FLCurrentConfiguration value.
	"Make a deep copy of the map"
	map := map class new
		addAll: map;
		yourself.
		
	self prepare
]

{ #category : 'initialization' }
FLHeaderSerializer >> prepare [
	#(
		object
		additionalObjects
		hasMultipleObjects
		preMaterializationActions
		postMaterializationActions
		substitutions
		#objectLimit
		depthLimit
	) do: [ :key |
		map
			removeKey: key
			ifAbsent: [] ]
]

{ #category : 'serializing' }
FLHeaderSerializer >> serialize [
	(FLCurrentConfiguration value isMemberOf: self class) ifTrue: [
		"prevent recursion"
		^ self ].
	
	self object: FLHeader forSerialization.
	
	super serialize
]

{ #category : 'asserting' }
FLHeaderSerializer >> shouldFinalizeStream [
	^ false
]

{ #category : 'serializing' }
FLHeaderSerializer >> useDuring: aBlock [
	| stream |
	stream := FLCurrentContext value stream.
	^ super useDuring: [
		FLCurrentContext value stream: stream.
		aBlock value ]
]
