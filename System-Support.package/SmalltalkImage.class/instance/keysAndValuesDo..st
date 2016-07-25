to clean later
keysAndValuesDo: aBlock
	"Obsoleted."
	self flag: #tobeprecated.
	"self deprecated: 'Use Smalltalk globals'."
	^globals keysAndValuesDo: aBlock
