drawing
newContentMorph
	"Answer a new content morph"

	^TextMorphForFieldView new
		contents: self defaultContents;
		margins: (2@0 corner: 2@1);
		borderStyle: (BorderStyle simple width: 0);
		vResizing: #shrinkWrap;
		hResizing: #spaceFill;
		autoFit: false;
		lock.