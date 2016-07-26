conversion
fromCStrings
	"Assume that the receiver represents a set of C strings and is teerminated by a empty string and convert it to a Smalltalk ordered collection of strings"

	| stream index char strings str |
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
		str size = 0
	] whileFalse.
	^strings