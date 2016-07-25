mirror primitives
object: anObject instVarAt: anIndex
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable. Essential for the
	 debugger. See  Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self object: anObject basicAt: anIndex - (self objectClass: anObject) instSize