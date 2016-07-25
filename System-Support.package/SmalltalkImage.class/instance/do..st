to clean later
do: aBlock
	"Obsoleted."
	self flag: #tobeprecated.
	"self deprecated: 'Use Smalltalk globals'."
	^globals do: aBlock
	
