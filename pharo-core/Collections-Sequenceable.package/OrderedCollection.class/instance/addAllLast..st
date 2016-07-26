adding
addAllLast: aCollection 
	"Add each element of aCollection at the end of the receiver. 
	Answer aCollection."

	aCollection do: [:each | self addLast: each].
	^aCollection