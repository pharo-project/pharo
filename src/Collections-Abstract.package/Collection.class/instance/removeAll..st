removing
removeAll: aCollection 
	"Remove each element of aCollection from the receiver. If successful for 
	each, answer aCollection. Otherwise create an error notification.
	ArrayedCollections cannot respond to this message."

	aCollection == self ifTrue: [^self removeAll].
	aCollection do: [:each | self remove: each].
	^ aCollection