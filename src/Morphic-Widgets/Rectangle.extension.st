Extension { #name : 'Rectangle' }

{ #category : '*Morphic-Widgets' }
Rectangle >> asLayoutFrame [
	"Answer an instance of LayoutFrame which uses fractions from receiver "

	^ LayoutFrame new
		leftFraction: self left;
		rightFraction: self right;
		topFraction: self top;
		bottomFraction: self bottom;
		yourself
]

{ #category : '*Morphic-Widgets' }
Rectangle >> privateSetCorner: bottomRight [
	corner := bottomRight
]

{ #category : '*Morphic-Widgets' }
Rectangle >> privateSetOrigin: topLeft [
	origin := topLeft
]
