Trait {
	#name : 'TSequenceableTruncatedToSize',
	#category : 'Collections-Abstract-Tests-Traits',
	#package : 'Collections-Abstract-Tests',
	#tag : 'Traits'
}

{ #category : 'requirements' }
TSequenceableTruncatedToSize >> collectionWith5Elements [

	^ self explicitRequirement
]

{ #category : 'tests' }
TSequenceableTruncatedToSize >> testTruncatedToSizeLargerThanCollection [

	| collection truncatedCollection |
	collection := self collectionWith5Elements.
	truncatedCollection := collection truncatedToSize: 1000.
	self assert: truncatedCollection equals: collection
]

{ #category : 'tests' }
TSequenceableTruncatedToSize >> testTruncatedToSizeResultHasCorrectElements [

	| collection truncatedCollection smallSize |
	smallSize := 3.

	collection := self collectionWith5Elements.
	truncatedCollection := collection truncatedToSize: smallSize.

	1 to: smallSize do: [ :i |
		self assert: (truncatedCollection at: i) equals: (collection at: i) ]
]

{ #category : 'tests' }
TSequenceableTruncatedToSize >> testTruncatedToSizeResultHasCorrectSize [

	| collection truncatedCollection |
	collection := self collectionWith5Elements.
	truncatedCollection := collection truncatedToSize: 3.
	self assert: truncatedCollection size equals: 3
]

{ #category : 'tests' }
TSequenceableTruncatedToSize >> testTruncatedToSizeSameAsCollection [

	| collection truncatedCollection |
	collection := self collectionWith5Elements.
	truncatedCollection := collection truncatedToSize: 5.
	self assert: truncatedCollection equals: collection
]
