drawing
fullDrawOn: aCanvas
	"preventing from drawing outside of me"
	super fullDrawOn: (aCanvas copyClipRect: self fullBounds).