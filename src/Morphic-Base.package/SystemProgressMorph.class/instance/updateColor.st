updating
updateColor
	"Callback from theme"
	
	self theme preferGradientFill ifFalse: [^ self].
	self fillStyle: (self theme progressFillStyleFor: self)