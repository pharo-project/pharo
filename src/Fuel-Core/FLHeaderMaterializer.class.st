"
I am a specialization of FLMaterializer that knows how to materialize the header.

I copy the current configuration and manipulated some values to make the configuration works for header materialization. I also prevent recursive header materialization.
"
Class {
	#name : 'FLHeaderMaterializer',
	#superclass : 'FLMaterializer',
	#category : 'Fuel-Core-Configuration',
	#package : 'Fuel-Core',
	#tag : 'Configuration'
}

{ #category : 'initialization' }
FLHeaderMaterializer >> initialize [
	super initialize.
	
	"Copy the current configuration"
	self copyFrom: FLCurrentConfiguration value.
	"Make a deep copy of the map"
	map := map class new
		addAll: map;
		yourself.
		
	self prepare
]

{ #category : 'serializing' }
FLHeaderMaterializer >> materialize [
	| materializedObjects |
	(FLCurrentConfiguration value isMemberOf: self class) ifTrue: [
		"prevent recursion"
		^ FLCurrentContext value materializedObjects
			addRoot: FLHeader forMaterialization;
			yourself ].
	
	
	
	materializedObjects := super materialize.
	materializedObjects root ifNil: [
		materializedObjects addRoot: FLHeader forMaterialization ].
	^ materializedObjects
]

{ #category : 'initialization' }
FLHeaderMaterializer >> prepare [
	#(
		object
		additionalObjects
		hasMultipleObjects
		preMaterializationActions
		postMaterializationActions
		shouldMaterializeHeaderOnly
	) do: [ :key |
		map
			removeKey: key
			ifAbsent: [] ]
]

{ #category : 'asserting' }
FLHeaderMaterializer >> shouldFinalizeStream [
	^ false
]

{ #category : 'serializing' }
FLHeaderMaterializer >> useDuring: aBlock [
	| stream |
	stream := FLCurrentContext value stream.
	^ super useDuring: [
		FLCurrentContext value stream: stream.
		aBlock value ]
]
