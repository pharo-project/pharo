drawing
drawDropShadowOn: aCanvas

	aCanvas 
		translateBy: self shadowOffset 
		during: [ :shadowCanvas |
			shadowCanvas roundShadowCornersOf: self during: [
				(shadowCanvas isVisible: self bounds) ifTrue: [
					shadowCanvas fillRectangle: self bounds fillStyle: self shadowColor
				]
			]
		].
