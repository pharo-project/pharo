navigation
nextPosition
	"answer the position which is right next to the receiver"
	
	^ self isAtSpanEnd 
		ifFalse: [  span positionAt: spanPosition + 1 ]
		ifTrue: [ span next startPosition ]