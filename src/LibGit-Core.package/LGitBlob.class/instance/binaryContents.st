private-accessing
binaryContents
	| bytes |
	"freeing not required. See #rawBuffer"
	bytes := ByteArray new: self rawSize.
	LibC memCopy: self rawBuffer to: bytes size: bytes size.
	^bytes