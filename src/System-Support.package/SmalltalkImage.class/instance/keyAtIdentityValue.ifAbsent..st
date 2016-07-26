to clean later
keyAtIdentityValue: anObject ifAbsent: aBlock
	"Obsoleted."

	self flag: #tobeprecated.
	"self deprecated: 'Use Smalltalk globals'."
	^globals keyAtIdentityValue: anObject ifAbsent: aBlock