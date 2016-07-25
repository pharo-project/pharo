instance creation
newLabelMorph
	"Answer a new label morph"

	^TextMorph new
		roundedCorners: #(1 4);
		margins: (2@1 corner: 2 @ -1);
		contents: 'groupbox';
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		lock