testing method dictionary
hasSelectorReferringTo: literal
	"Answer true if any of my methods access the argument as a 
	literal. Dives into the compact literal notation, making it slow but 
	thorough "

	^self methods anySatisfy: [ :method | method hasLiteral: literal ]
		