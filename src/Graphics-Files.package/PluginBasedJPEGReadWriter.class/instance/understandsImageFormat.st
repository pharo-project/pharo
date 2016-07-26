testing
understandsImageFormat
	"Answer true if the image stream format is understood by this decoder."
	self isPluginPresent ifFalse:[^false]. "cannot read it otherwise"
	self next = 16rFF ifFalse: [^ false].
	self next = 16rD8 ifFalse: [^ false].
	^ true
