mirror primitives
objectSize: anObject
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0