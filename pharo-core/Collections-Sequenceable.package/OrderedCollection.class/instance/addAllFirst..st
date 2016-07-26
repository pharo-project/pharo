adding
addAllFirst: anOrderedCollection 
	"Add each element of anOrderedCollection at the beginning of the 
	receiver. Answer anOrderedCollection."

	anOrderedCollection reverseDo: [:each | self addFirst: each].
	^anOrderedCollection