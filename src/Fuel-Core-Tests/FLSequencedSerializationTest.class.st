Class {
	#name : 'FLSequencedSerializationTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-FullSerialization',
	#package : 'Fuel-Core-Tests',
	#tag : 'FullSerialization'
}

{ #category : 'tests' }
FLSequencedSerializationTest >> assertSequencedSerializationEqualityOf: aCollectionToSerialize [	
	| materializedCollection |
	self serializer
		objectCollection: aCollectionToSerialize;
		serialize.
	
	materializedCollection := self materializer materializeObjectCollection roots.
		
	self assertCollection: aCollectionToSerialize hasSameElements: materializedCollection
]

{ #category : 'initialization' }
FLSequencedSerializationTest >> setUpStreamStrategy [
	streamStrategy := FLByteArrayStreamStrategy new
]

{ #category : 'tests' }
FLSequencedSerializationTest >> testComplexObjects [
	self assertSequencedSerializationEqualityOf: {
		Class.
		FLPair.
		FLPair new
			left: 1;
			right: 2.
		FLPair methodDictionary.
		Smalltalk globals }
]

{ #category : 'tests' }
FLSequencedSerializationTest >> testPrimitiveObjects [
	self assertSequencedSerializationEqualityOf: (1 to: 50) asOrderedCollection.
]
