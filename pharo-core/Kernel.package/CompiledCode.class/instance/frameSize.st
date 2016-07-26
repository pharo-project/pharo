accessing
frameSize
	"Answer the size of temporary frame needed to run the receiver."
	"NOTE:  Versions 2.7 and later use two sizes of contexts."

	(self header noMask: 16r20000)
		ifTrue: [^ SmallFrame]
		ifFalse: [^ LargeFrame]
