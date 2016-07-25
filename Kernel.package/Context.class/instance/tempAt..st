accessing
tempAt: index 
	"Answer the value of the temporary variable whose index is the 
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the
	 value of an indexable element in the receiver. Fail if the argument index
	 is not an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default at: primitive to give latitude to the
	 VM in context management."

	<primitive: 210>
	^self at: index