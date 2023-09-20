"
SUnit tests for basic serialization that is zipped
"
Class {
	#name : 'FLGZippedBasicSerializationTest',
	#superclass : 'FLBasicSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
FLGZippedBasicSerializationTest >> setUpStreamStrategy [
	streamStrategy := FLGZipStrategy newWithTarget: FLByteArrayStreamStrategy new
]

{ #category : 'tests' }
FLGZippedBasicSerializationTest >> testByteArray [

	self assertSerializationEqualityOf: #[1 2 3 4 5 6 7]
]

{ #category : 'tests' }
FLGZippedBasicSerializationTest >> testCompleteBinaryTree [

	| aCollection numberOfLeafs |
	numberOfLeafs := 2 raisedTo: 13.
	aCollection := OrderedCollection new.
	1 to: numberOfLeafs do: [ :i | 
		aCollection add: (
			FLPair new 
				left: (2 * i); 
				right: (2 * i) + 1;
				yourself) ].
	
	[aCollection size > 1] 
		whileTrue: [
			aCollection := aCollection pairsCollect: [ :leftPair :rightPair | 
				FLPair new 
					left: leftPair;
					right: rightPair;
					yourself ] ].

	self assertSerializationEqualityOf: aCollection
]

{ #category : 'tests' }
FLGZippedBasicSerializationTest >> testPairs [

	| pairs |
	pairs := (1 to: 1 << 14) 
		collect: [ :i | 
			FLPair new 
				left: i; 
				right: i+1;
				yourself ].

	self assertSerializationEqualityOf: pairs
]

{ #category : 'tests-strings' }
FLGZippedBasicSerializationTest >> testWideString [
	| wideString collection |
	wideString := WideString streamContents: [ :stream |
		256
			to: 1000
			do: [ :int | stream nextPut: int asCharacter ] ].

	collection := Array
		new: 600
		streamContents: [ :stream |
			600 timesRepeat: [
				stream nextPut: wideString ] ].

	self assertSerializationEqualityOf: collection
]
