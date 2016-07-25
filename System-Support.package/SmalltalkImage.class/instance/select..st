to clean later
select: aBlock
	"Obsoleted."
	self flag: #tobeprecated.
	"self deprecated: 'Use Smalltalk globals'."
	^globals select: aBlock
