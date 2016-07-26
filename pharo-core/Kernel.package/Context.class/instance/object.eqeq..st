mirror primitives
object: anObject eqeq: anOtherObject 
	"Answer whether the first and second arguments are the same object (have the
	 same object pointer) without sending a message to the first argument.  This
	 mimics the action of the VM when it compares two object pointers.  Used to
	 simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 110>
	self primitiveFailed