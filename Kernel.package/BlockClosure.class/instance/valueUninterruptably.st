exceptions
valueUninterruptably
	"Prevent remote returns from escaping the sender.  Even attempts to terminate (unwind) this process will be halted and the process will resume here.  A terminate message is needed for every one of these in the sender chain to get the entire process unwound."

	^ self ifCurtailed: [^ self]