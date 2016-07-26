initialization
initializeAppearance
	"Initialize the appearance."

	self
		color: (self theme textColor alpha: 0.3);
		fillStyle: (self theme taskbarFillStyleFor: self)