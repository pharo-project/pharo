to clean later
removeKey: key
	"Obsoleted."

	self flag: #tobeprecated.
	"self deprecated: 'Use Smalltalk globals'."
	^globals removeKey: key