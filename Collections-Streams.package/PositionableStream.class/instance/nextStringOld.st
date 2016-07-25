nonhomogeneous accessing
nextStringOld
	"Read a string from the receiver. The first byte is the length of the 
	string, unless it is greater than 192, in which case the first *two* bytes 
	encode the length.  Max size 16K. "

	| aString length |
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := (length - 192) * 256 + self next].
	aString := String new: length.
	1 to: length do: [:ii | aString at: ii put: self next asCharacter].
	^aString