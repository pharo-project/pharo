testing
understandsImageFormat
	"Answer true if the image stream format is understood by this decoder."
	self next = 16rFF ifFalse: [^ false].
	self next = 16rD8 ifFalse: [^ false].
	^ true
