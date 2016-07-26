to clean later
keyAtValue: anObject
	"Obsoleted."
	self flag: #tobeprecated.
	"self deprecated: 'Use Smalltalk globals'."
	^globals keyAtValue: anObject