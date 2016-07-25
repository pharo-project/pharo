converting
unstyledTextFrom: aText 
	"return a Text object without all styling attributes"
	| answer |
	answer := super unstyledTextFrom: aText.
	
	self 
		addAttributesFrom: aText runs 
		satisfying: [ :each| each isKindOf: TextAction ]
		to: answer.
	
	^ answer