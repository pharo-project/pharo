"
SUnit tests for basic serialization that is zipped
"
Class {
	#name : #FLGZippedBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #'expected failures' }
FLGZippedBasicSerializationTest >> expectedFailures [

	^ super expectedFailures, #(testWideString)
]

{ #category : #running }
FLGZippedBasicSerializationTest >> setUp [
	super setUp.
	self useGzipInMemoryStream.
]

{ #category : #tests }
FLGZippedBasicSerializationTest >> testByteArray [

	self assertSerializationEqualityOf: #[1 2 3 4 5 6 7]
]

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
FLGZippedBasicSerializationTest >> testWideString [

	| anObject |
	anObject := OrderedCollection new.
	600 timesRepeat: [
	       anObject add: (WideString
	               streamContents: [ :stream |
	                       600 timesRepeat: [
	                               stream nextPut: (256 to: 1000) atRandom asCharacter ] ] ) ].
	
	self
		shouldnt: [ self assertSerializationEqualityOf: anObject ]
		raise: Error
]
