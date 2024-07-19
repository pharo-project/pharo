Extension { #name : 'ExternalData' }

{ #category : '*Deprecated12' }
ExternalData >> fromCString [
	self
		deprecated:  'Please use #bytesFromCString instead'
		transformWith:  '`@receiver fromCString'
						-> '`@receiver bytesFromCString utf8Decoded'.

	type isPointerType ifFalse: [self error: 'External object is not a pointer type.'].

	^ handle fromCString
]

{ #category : '*Deprecated12' }
ExternalData >> fromCStrings [
	"Assume that the receiver represents a set of C strings and is terminated by a empty string and convert it to a Smalltalk ordered collection of strings"

	| stream index char strings str |

	self
		deprecated:  'This method returns strings that are not decoded. Please use #bytesFromCString with the rifght decoder instead'.

	type isPointerType ifFalse: [self error: 'External object is not a pointer type.'].
	strings := OrderedCollection new.
	index := 1.
	[
		stream := WriteStream on: String new.
		[(char := handle unsignedCharAt: index) = 0 asCharacter]
			whileFalse: [
				stream nextPut: char.
				index := index + 1
			].
		str := stream contents.
		strings addLast: str.
		str isEmpty
	] whileFalse.
	^strings
]
