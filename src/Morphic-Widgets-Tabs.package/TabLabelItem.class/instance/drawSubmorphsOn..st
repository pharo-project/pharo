drawing
drawSubmorphsOn: aCanvas
	"Display submorphs back to front"

	| drawBlock submorphsToDraw |
	submorphs isEmpty
		ifTrue: [ ^ self ].
	
	submorphsToDraw := self computeSubmorphsToDraw.
	
	drawBlock := [ :canvas | submorphsToDraw do: [ :m | canvas fullDrawMorph: m ] ].
	
	self clipSubmorphs
		ifTrue: [ aCanvas clipBy: (aCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock ]
		ifFalse: [ drawBlock value: aCanvas ]