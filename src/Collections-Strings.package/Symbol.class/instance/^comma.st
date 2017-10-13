copying
, otherSymbolOrString
	"Concatenate the receiver with otherSymbolOrString and return the result.
	Overwritten to always return Symbol results,
	whether the argument is a Symbol or a String"
	
	^ (super , otherSymbolOrString) asSymbol