accessing
at: anInteger
	"Answer the anInteger'th element."

	(anInteger between: 1 and: self size)
		ifTrue: [ ^ start + (step * (anInteger - 1)) ]
		ifFalse: [ self errorSubscriptBounds: anInteger ]