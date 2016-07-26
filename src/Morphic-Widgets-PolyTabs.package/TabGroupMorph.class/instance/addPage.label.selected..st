adding
addPage: aMorph label: aStringOrMorph selected: selectedStringOrMorph
	"Add a page and its tab with alternate label when selected."

	aMorph
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	self pages add: aMorph.
	self tabSelectorMorph addTab: aStringOrMorph selected: selectedStringOrMorph