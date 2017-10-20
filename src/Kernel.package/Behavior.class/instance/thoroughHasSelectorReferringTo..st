testing method dictionary
thoroughHasSelectorReferringTo: literal
	"Answer true if any of my methods access the argument as a 
	literal. Dives into the compact literal notation, making it slow but 
	thorough "
	
	| specialIndex |
	"for speed we check the special selectors here once per class"
	specialIndex := Smalltalk specialSelectorIndexOrNil: literal.
	^self methods anySatisfy: [ :method | 
		method hasSelector: literal specialSelectorIndex: specialIndex]
		