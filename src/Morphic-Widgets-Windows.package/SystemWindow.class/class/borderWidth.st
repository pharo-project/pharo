initializing
borderWidth

	"Making changes to this for some reason requires repositioning of CornerGripMorphs.
	Edit BorderedMorph#addCornerGrip and play with offsets to get them right if you increase
	border width. For instance, going from 4 to 6 here and you should updated offsets to
	(-23@-23 corner: 0@0) for the right placement of corner grips."

	^ self theme windowBorderWidthFor: self