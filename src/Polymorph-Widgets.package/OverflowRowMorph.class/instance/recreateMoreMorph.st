as yet unclassified
recreateMoreMorph
	"Answer a new morph showing the undisplayed base morphs.
	Assign to moreMorph."

	self moreMorph: (self theme builder newColumn: self moreMorphs).
	self moreMorph
		setProperty: #morphicLayerNumber toValue: 6;
		layoutInset: 4;
		borderStyle: (self theme taskbarThumbnailNormalBorderStyleFor: self moreMorph);
		extent: self moreMorph minExtent;
		position: self desiredMoreMorphPosition;
		paneColor: self paneColor;
		color: self paneColor.
	^self moreMorph