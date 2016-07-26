drawing
drawOnFormCanvas: aCanvas 
	"Display the receiver, a spline curve, approximated by straight line segments."

	vertices size < 1 ifTrue: [self error: 'a polygon must have at least one point'].
	closed & color isTransparent not
		ifTrue: [aCanvas stencil: self filledForm at: bounds topLeft - 1 color: color].
	(borderColor isColor and: [borderColor isTranslucentButNotTransparent])
		ifTrue: [aCanvas stencil: self borderForm at: bounds topLeft color: borderColor]
		ifFalse: [self drawBorderOn: aCanvas].
	self arrowForms do:
		[:f | aCanvas 
				stencil: f 
				at: f offset
				color: (borderColor isColor ifTrue: [borderColor] ifFalse: [color])]