to clean later
includes: element
	"Obsoleted."

	self flag: #tobeprecated.
	"self deprecated: 'Use Smalltalk globals'."
	^globals includes: element