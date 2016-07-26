private
smartNeedExtraRemoveIn: currentText for: opposite
	"Test if smart remove need to remove an extra character when the smart character 
	 is equal to its opposite"
	
	(currentText select: [ :char | char = opposite ]) size odd
		ifTrue: [ ^ false ].
	^true