fields accessing
fixedFieldOf: anObject at: anIndex put: aValue 
	"Primitive. Store a value into a fixed variable in the argument anObject.
	 The numbering of the variables corresponds to the named instance
	 variables.  Fail if the index is not an Integer or is not the index of a
	 fixed variable.  Answer the value stored as the result"

	<primitive: 74>
	"Access beyond fixed fields"
	^self primitiveFail 