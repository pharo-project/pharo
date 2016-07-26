mirror primitives
object: anObject instVarAt: anIndex put: aValue 
	"Primitive. Store a value into a fixed variable in the argument anObject.
	 The numbering of the variables corresponds to the named instance
	 variables.  Fail if the index is not an Integer or is not the index of a
	 fixed variable.  Answer the value stored as the result. Using this
	 message violates the  principle that each object has sovereign control
	 over the storing of values into its instance variables. Essential for the
	 debugger. See Object documentation whatIsAPrimitive."

	<primitive: 74>
	"Access beyond fixed fields"
	^self object: anObject basicAt: anIndex - (self objectClass: anObject) instSize put: aValue