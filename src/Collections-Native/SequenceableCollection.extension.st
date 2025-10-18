Extension { #name : 'SequenceableCollection' }

{ #category : '*Collections-Native' }
SequenceableCollection >> asByteArray [
	"Answer a ByteArray whose elements are the elements of the receiver."

	^ ByteArray withAll: self
]

{ #category : '*Collections-Native' }
SequenceableCollection >> asFloatArray [
	"Answer a FloatArray whose elements are the elements of the receiver, in
	the same order."

	| floatArray |
	floatArray := Float32Array new: self size.
	1 to: self size do:[:i| floatArray at: i put: (self at: i) asFloat ].
	^floatArray
]

{ #category : '*Collections-Native' }
SequenceableCollection >> asIntegerArray [
	"Answer an IntegerArray whose elements are the elements of the receiver, in
	the same order."

	| intArray |
	intArray := IntegerArray new: self size.
	1 to: self size do:[:i| intArray at: i put: (self at: i)].
	^intArray
]

{ #category : '*Collections-Native' }
SequenceableCollection >> asWordArray [
	"Answer a WordArray whose elements are the elements of the receiver, in
	the same order."

	| wordArray |
	wordArray := WordArray new: self size.
	1 to: self size do:[:i| wordArray at: i put: (self at: i)].
	^wordArray
]
