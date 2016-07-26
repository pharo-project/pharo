private
streamBuffer: size
	"Create a buffer of the correct class and given size."

	^(self isBinary
		ifTrue: [ByteArray]
		ifFalse: [String]) new: size