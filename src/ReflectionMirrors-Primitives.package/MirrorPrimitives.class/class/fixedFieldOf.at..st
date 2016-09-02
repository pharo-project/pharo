fields accessing
fixedFieldOf: anObject at: anIndex
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable"

	<primitive: 73>
	"Access beyond fixed variables."
	^self primitiveFailed