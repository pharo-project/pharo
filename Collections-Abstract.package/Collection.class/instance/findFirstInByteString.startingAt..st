enumerating
findFirstInByteString: aByteString startingAt: start
	"Find the index of first character starting at start in aByteString that is included in the receiver.
	Default is to use a naive algorithm.
	Subclasses might want to implement a more efficient scheme"

	start to: aByteString size do:
		[:index |
		(self includes: (aByteString at: index)) ifTrue: [^ index]].
	^ 0