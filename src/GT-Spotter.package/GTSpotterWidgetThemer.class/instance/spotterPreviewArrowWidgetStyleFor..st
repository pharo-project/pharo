styles-widget
spotterPreviewArrowWidgetStyleFor: aBrick

	aBrick owner isTriangleVisible
		ifTrue: [
			aBrick color: self previewArrowColor.
			aBrick renderer borderColor: self borderColor ]
		ifFalse: [
			aBrick color: Color transparent.
			aBrick renderer borderColor: Color transparent ].
	
	GTSpotter isPreviewVisible
		ifTrue: [ 
			aBrick selectedColor: self borderColor.
			aBrick renderer border: 0 ]
		ifFalse: [
			aBrick selectedColor: Color transparent.
			aBrick renderer border: self outerBorderWidth ]