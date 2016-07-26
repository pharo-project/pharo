to clean later
removeKey: key ifAbsent: aBlock
	"Obsoleted."

	self flag: #tobeprecated.
	"self deprecated: 'Use Smalltalk globals'."
	^globals removeKey: key ifAbsent: aBlock