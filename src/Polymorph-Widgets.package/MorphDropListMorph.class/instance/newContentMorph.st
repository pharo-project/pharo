drawing
newContentMorph
	"Answer a new content morph"

	^Morph new
		changeTableLayout;
		listDirection: #leftToRight;
		wrapCentering: #center;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		layoutInset: 2;
		color: Color transparent;
		borderWidth: 0;
		clipSubmorphs: true;
		lock