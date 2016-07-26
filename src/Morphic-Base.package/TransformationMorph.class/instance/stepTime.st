testing
stepTime
	"Answer the stepTime of my rendered morph if posible"

	| rendered |
	rendered := self renderedMorph.
	rendered = self ifTrue: [^super stepTime].	"Hack to avoid infinite recursion"
	^rendered stepTime.
	