event handling
updateFromEvent: anEvent 
	"Update the splitter and attached morph positions from
	the mouse event.
	Take into account the mouse down offset."
	| delta |
	
	delta := (anEvent cursorPoint - lastMouse first) x.
	"if positive, delta cannot be bigger than right width (plus a variation) 
	 to prevent column overlap."
	(delta > 0 and: [ delta > (rightColumn width - (self width) - 5) ])
		ifTrue: [ delta := rightColumn width - (self width) - 5 ].
	"if negative, -delta cannot be bigger than left width (plus a variation) 
	 to prevent column overlap."
	(delta < 0 and: [ delta negated > (leftColumn width - (self width) - 5) ])
		ifTrue: [ delta := (leftColumn width - (self width) - 5) negated ].

	leftColumn column width: (leftColumn column width ifNil: [ leftColumn width ]) + delta.
	rightColumn column width: (rightColumn column width ifNil: [ rightColumn width ]) - delta.	
	container changed