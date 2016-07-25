printing
printOn: aStream base: b
	"Append a representation of this number in base b on aStream."
	
	aStream nextPut: $-.
	self abs printOn: aStream base: b