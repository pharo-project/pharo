instance creation
newContentMorph
	"Answer a new content morph"

	|p|
	p := GLMPanelMorph new
		roundedCorners: #(2 3 4);
		changeTableLayout; 
		layoutInset: 4;
		cellInset: 8;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	p borderStyle: (self theme tabPanelBorderStyleFor: self).
	^p