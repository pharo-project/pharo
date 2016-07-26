*Athens-Morphic
drawSubmorphsOnAthensCanvas: anAthensCanvas 
	"Display submorphs back to front"

	| drawBlock|
	submorphs isEmpty ifTrue: [^self].
	drawBlock := [:canvas | | i |
		(self topVisibleRowForCanvas: anAthensCanvas) to: (self bottomVisibleRowForCanvas: anAthensCanvas) do: [ :row |
			i := self item: row.
			canvas fullDrawMorph: i]].
	self clipSubmorphs 
		ifTrue: [anAthensCanvas clipBy: (anAthensCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock]
		ifFalse: [drawBlock value: anAthensCanvas]