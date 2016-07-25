events
updateColor
	"Update the color of the menu."

	self theme preferGradientFill
		ifFalse: [ ^ self ].
	self fillStyle: (self theme menuFillStyleFor: self).	
	"update the title color"
	
	titleMorph ifNotNil: [ :tm | tm fillStyle: (self theme menuTitleFillStyleFor: tm )]
