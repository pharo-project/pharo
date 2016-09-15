convenience
newTreeColumnModel
	"Answer a TreeColumnModel ready to be used in a TreeModel."
	
	^ TreeColumnModel new	
		displayBlock: [ :item | self rowMorphFor: item content ];
		yourself.	