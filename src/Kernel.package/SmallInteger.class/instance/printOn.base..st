printing
printOn: stream base: base 
	"Append a representation of this number in base b on aStream."

	self printOn: stream base: base length: 0 padded: false