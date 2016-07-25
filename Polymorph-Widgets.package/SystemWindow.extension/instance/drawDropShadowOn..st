*Polymorph-Widgets
drawDropShadowOn: aCanvas
	"Get the theme to draw the drop shawdow for the receiver."
	
	|dropAreas|
	dropAreas := self areasRemainingToFill: (self bounds expandBy: self shadowOffsetRectangle).
	(dropAreas anySatisfy: [:rect | aCanvas isVisible: rect])
		ifFalse: [^self]. "no need to draw since no intersection"
	self isActive
		ifTrue: [self theme
					drawWindowActiveDropShadowFor: self
					on: aCanvas]
		ifFalse: [self theme
					drawWindowInactiveDropShadowFor: self
					on: aCanvas]