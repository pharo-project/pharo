testing
isOctetString
	"Answer whether the receiver can be represented as a byte string. 
	This is different from asking whether the receiver *is* a ByteString 
	(i.e., #isByteString)"
	1 to: self size do: [:pos |
		(self at: pos) asInteger >= 256 ifTrue: [^ false].
	].
	^ true.
