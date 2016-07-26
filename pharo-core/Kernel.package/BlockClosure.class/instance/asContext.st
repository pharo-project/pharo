scheduling
asContext
	"Create a MethodContext that is ready to execute self.  Assumes self takes no args (if it does the args will be nil)"

	^self asContextWithSender: nil