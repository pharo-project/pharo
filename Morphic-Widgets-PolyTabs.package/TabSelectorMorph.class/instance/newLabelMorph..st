operations
newLabelMorph: aStringOrMorph
	"Answer a new label morph with the given label text."

	^TabLabelMorph new
		roundedCorners: #(1 4);
		cornerStyle: self cornerStyle;
		changeTableLayout;
		listDirection: #leftToRight;
		listCentering: #center;
		layoutInset: (self theme tabLabelInsetFor: self);
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		label: aStringOrMorph;
		font: self font;
		on: #mouseDown send: #tabClicked:with: to: self;
		tabSelector: self