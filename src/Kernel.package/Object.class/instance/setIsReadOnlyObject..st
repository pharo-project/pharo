write barrier
setIsReadOnlyObject: aBoolean
	"If the VM supports read-only objects it will not write to read-only objects.
	 An attempt to write to an instance variable of a read-only object will
	 cause the VM to send attemptToAssign:withIndex: to the read-only object.
	 An attempt to modify a read-only object in a primitive will cause the
	 primitive to fail with a #'no modification' error code.
	 This primitive sets the read-only flag of the receiver to the given
	 value and answers the previous vaue of the flag.
	 Note: Some objects can't be read-only, currently contexts and objects related
	 to process scheduling (Processor, Process instances, Semaphore instances, ...)"
	<primitive: 164 error: ec>
	^self primitiveFailed
	