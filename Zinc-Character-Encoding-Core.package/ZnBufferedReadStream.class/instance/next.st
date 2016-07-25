accessing
next
	"Return the next element and move over it"
	
	position > limit
		ifTrue: [ self nextBuffer ].
	^ position <= limit
		ifTrue: [ 
			| char |
			char := buffer at: position.
			position := position + 1.
			char ]
		ifFalse: [ nil ]