instance creation
newStdCall: argBytes
	"Answer a new instance for Windows' StdCall (Pascal) convention, given the
	 size of the arguments in bytes (bytes to cut-back from the stack on return)."
	^self privateFinalizingNew initializeStdcall: argBytes