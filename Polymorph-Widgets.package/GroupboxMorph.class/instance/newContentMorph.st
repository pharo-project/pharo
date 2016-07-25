instance creation
newContentMorph
	"Answer a new content morph"

	|p|
	p := PanelMorph new
		roundedCorners: self roundedCorners;
		changeTableLayout; 
		layoutInset: 4;
		cellInset: 8;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	p borderStyle: (self theme groupPanelBorderStyleFor: p).
	^p