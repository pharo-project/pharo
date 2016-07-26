drawing
drawSubmorphsOn: aCanvas
	"Display submorphs back to front, but skip my background sketch."

	| drawBlock |
	submorphs isEmpty
		ifTrue: [ ^ self ].
	drawBlock := [ :canvas | 
	submorphs
		reverseDo: [ :m | 
			m ~~ backgroundMorph
				ifTrue: [ canvas fullDrawMorph: m ] ] ].
	self clipSubmorphs
		ifTrue: [ aCanvas clipBy: (aCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock ]
		ifFalse: [ drawBlock value: aCanvas ]