Extension { #name : #LazyMorphListMorph }

{ #category : #'*Athens-Morphic' }
LazyMorphListMorph >> athensDisplay: item atRow: row on: aCanvas [
	"Display the given item at the given row on the given canvas."

	row = self mouseDownRow
		ifTrue: [
			(aCanvas setStrokePaint: (self colorForRow: row))
				width: 1;
				dashes: {1} offset: 0.
			aCanvas drawShape: (self selectionFrameForRow: row) ]
]

{ #category : #'*Athens-Morphic' }
LazyMorphListMorph >> drawSubmorphsOnAthensCanvas: anAthensCanvas [
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
]
