*System-Finalization
primitiveFetchMourner
	"Answer the next mourner in the VM's queue of objects to be finalized.
	 The queue contains weak arrays and ephemerons.  If the primitive is
	 not implemented, raise an error telling people to upgrade the VM.  If
	 implemented, the primitive fails if the queue is empty, with the error
	 code #'not found'. Primitive.  Essential."

	<primitive: 172 error: ec>
	ec ifNil: [^self error: 'The primitiveFetchMourner primitive is missing.\Please upgrade your virtual machine to one that has the primitive.' withCRs].
	^nil