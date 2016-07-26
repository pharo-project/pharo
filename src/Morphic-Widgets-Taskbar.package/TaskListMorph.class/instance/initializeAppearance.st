initialization
initializeAppearance
	"Initialize the appearance."

	self
		color: (Color black alpha: 0.15);
		fillStyle: (self theme taskListFillStyleFor: self);
		borderStyle: (self theme taskbarThumbnailNormalBorderStyleFor: self);
		cornerStyle: (self theme taskbarThumbnailCornerStyleFor: self)