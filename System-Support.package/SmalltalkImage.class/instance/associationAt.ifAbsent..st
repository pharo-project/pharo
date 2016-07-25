to clean later
associationAt: key ifAbsent: aBlock
	"Obsoleted."

	self flag: #tobeprecated.
	"self deprecated: 'Use Smalltalk globals'."
	^globals associationAt: key ifAbsent: aBlock