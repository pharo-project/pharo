accessing
tempAt: index put: value 
	"Store the argument, value, as the temporary variable whose index is the 
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the
	 value of an indexable element in the receiver. Fail if the argument index
	 is not an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default at:put: primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	^self at: index put: value