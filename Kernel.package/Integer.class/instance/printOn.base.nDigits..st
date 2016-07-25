printing-numerative
printOn: aStream base: b nDigits: n 
	"Append a representation of this number in base b on aStream using nDigits.
	self must be positive."

	self subclassResponsibility