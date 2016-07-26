as yet unclassified
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front"

	| drawBlock|
	submorphs isEmpty ifTrue: [^self].
	drawBlock := [:canvas | | i |
		(self topVisibleRowForCanvas: aCanvas) to: (self bottomVisibleRowForCanvas: aCanvas) do: [ :row |
			i := self item: row.
			canvas fullDrawMorph: i]].
	self clipSubmorphs 
		ifTrue: [aCanvas clipBy: (aCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock]
		ifFalse: [drawBlock value: aCanvas]