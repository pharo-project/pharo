as yet unclassified
baseMorph: aRowMorph
	"Configure the receiver to match the specifications for the given morph and
	store the given morph's submorphs for later reallocation."
	
	self baseMorphs: aRowMorph submorphs.
	self
		cellInset: aRowMorph cellInset;
		layoutInset: aRowMorph layoutInset;
		height: aRowMorph minExtent y;
		minHeight: self height.
	self fitBaseMorphs