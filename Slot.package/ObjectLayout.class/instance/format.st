format
format
	"Answer an Integer that encodes this layout in the way that VM expects it."
	
	| fieldSize  format |
	fieldSize := self fieldSize.
	format := self instanceSpecification.
	format := (format bitShift: 16) + fieldSize.
	^ format