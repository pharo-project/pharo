drawing-images
image: aForm at: aPoint sourceRect: sourceRect rule: rule
	"Draw the given form. For the 'paint' combination rule use stenciling otherwise simply fill the source rectangle.
	Do a blendAlpha if the rule is blend to cope with translucent images being drawn (via translucentImage:...)."
	
	rule = Form paint ifTrue:[
		^myCanvas
			image: aForm
			at: aPoint
			sourceRect: sourceRect
			rule: Form paintAlpha
			alpha: alpha.
	].
	rule = Form over ifTrue:[
		^myCanvas
			image: aForm
			at: aPoint
			sourceRect: sourceRect
			rule: Form blendAlpha
			alpha: alpha.
	].
	rule = Form blend ifTrue:[
		^myCanvas
			image: aForm
			at: aPoint
			sourceRect: sourceRect
			rule: Form blendAlpha
			alpha: alpha.
	].