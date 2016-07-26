drawing
drawSubmorphsOnAthensCanvas: anAthensCanvas
	"Display submorphs back to front"

	| drawBlock subs |
	submorphs isEmpty
		ifTrue: [ ^ self ].
	subs := submorphs copyWithoutAll: self sideRulers.
	drawBlock := [ subs reverseDo: [ :m | anAthensCanvas fullDrawMorph: m ] ].
	self clipSubmorphs
		ifTrue: [ anAthensCanvas clipBy: (anAthensCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock ]
		ifFalse: [ drawBlock value ].
	subs := self sideRulers.
	drawBlock := [ subs reverseDo: [ :m | anAthensCanvas fullDrawMorph: m ] ].
	self clipSubmorphs
		ifTrue: [ anAthensCanvas clipBy: (anAthensCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock ]
		ifFalse: [ drawBlock value ]