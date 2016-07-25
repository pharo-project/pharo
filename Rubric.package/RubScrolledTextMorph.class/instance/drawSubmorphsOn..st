drawing
drawSubmorphsOn: aCanvas
	"Display submorphs back to front"

	| drawBlock subs |
	submorphs isEmpty
		ifTrue: [ ^ self ].
	subs := submorphs copyWithoutAll: self sideRulers.
	drawBlock := [ :canvas | subs reverseDo: [ :m | canvas fullDrawMorph: m ] ].
	self clipSubmorphs
		ifTrue: [ aCanvas clipBy: (aCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock ]
		ifFalse: [ drawBlock value: aCanvas ].
	subs := self sideRulers.
	drawBlock := [ :canvas | subs reverseDo: [ :m | canvas fullDrawMorph: m ] ].
	self clipSubmorphs
		ifTrue: [ aCanvas clipBy: (aCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock ]
		ifFalse: [ drawBlock value: aCanvas ]