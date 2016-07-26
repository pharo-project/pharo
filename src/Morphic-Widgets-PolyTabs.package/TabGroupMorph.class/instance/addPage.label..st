adding
addPage: aMorph label: aStringOrMorph
	"Add a page and its tab."

	aMorph
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	self pages add: aMorph.
	self tabSelectorMorph addTab: aStringOrMorph