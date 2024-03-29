Extension { #name : 'Collection' }

{ #category : '*Collections-Native' }
Collection >> asByteArray [
	"Answer a ByteArray whose elements are the elements of the receiver.
	Implementation note: Cannot use ''ByteArray withAll: self'' as that only
	works for SequenceableCollections which support the replacement
	primitive."

	"{1. 2} asByteArray >>> #[1 2]"
	"#[1 2] asByteArray == #[1 2] >>> true"
	"'foo' asByteArray >>> #[102 111 111]"
	"#() asByteArray >>> #[]"

	| array index |
	array := ByteArray new: self size.
	index := 0.
	self do: [:each | array at: (index := index + 1) put: each].
	^ array
]
