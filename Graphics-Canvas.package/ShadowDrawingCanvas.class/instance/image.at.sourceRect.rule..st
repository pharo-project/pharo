private
image: aForm at: aPoint sourceRect: sourceRect rule: rule
	"Draw the given form. For the 'paint' combination rule use stenciling otherwise simply fill the source rectangle."
	rule = Form paint ifTrue:[
		^myCanvas
			stencil: aForm
			at: aPoint
			sourceRect: sourceRect
			color: shadowColor
	] ifFalse:[
		^myCanvas
			fillRectangle: (sourceRect translateBy: aPoint)
			color: shadowColor
	].