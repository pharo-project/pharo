write barrier
isReadOnlyObject
	"Answer if the receiver is read-only.
	 If the VM supports read-only objects it will not write to read-only objects.
	 An attempt to write to an instance variable of a read-only object will
	 cause the VM to send attemptToAssign:withIndex: to the read-only object.
	 An attempt to modify a read-only object in a primitive will cause the
	 primitive to fail with a #'no modification' error code."
	<primitive: 163 error: ec>
	^self primitiveFailed