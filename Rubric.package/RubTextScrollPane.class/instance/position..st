geometry
position: aPoint
	"Update the receiver's extent. Hide/show the scrollbars and resize the scroller
	as neccessary."
	| previousBounds |
	previousBounds := self bounds copy.
	previousBounds topLeft = aPoint
		ifTrue: [ ^ self ].
	super position: aPoint