drawing
drawBackgroundSketchOn: aCanvas
	backgroundMorph ifNil: [ ^ self ].
	self clipSubmorphs
		ifTrue: [ aCanvas clipBy: self clippingBounds during: [ :canvas | canvas fullDrawMorph: backgroundMorph ] ]
		ifFalse: [ aCanvas fullDrawMorph: backgroundMorph ]