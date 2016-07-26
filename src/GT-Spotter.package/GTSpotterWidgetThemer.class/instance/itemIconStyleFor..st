styles
itemIconStyleFor: aBrick
	"icon form must already be set before styling"

	self flag: 'figure out why alpha doesn''t work'.
	aBrick
		"making icon a bit transparent"
		"normalImage: (aBrick normalImage asAlphaImageMorph alpha: self itemIconAlphaValue) imageForm;"
		width: self itemIconMaxWidth;
		vSpaceFill