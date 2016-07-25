extended bytecode generation
genPushFullClosure: compiledBlockLiteralIndex numCopied: numCopied
	"By default the closure will have an outer context and the receiver will be fetched from the current context"
	self genPushFullClosure: compiledBlockLiteralIndex numCopied: numCopied receiverOnStack: false outerContextNeeded: true