initialize-release
receiver: rcvr selector: selNode arguments: args precedence: p 
	"Decompile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := selNode key.
	selNode code == #macro
		ifTrue: [self noteSpecialSelector: selNode key]
		ifFalse: [special := 0].
	selector := selNode.
	"self pvtCheckForPvtSelector: encoder"
	"We could test code being decompiled, but the compiler should've checked already. And where to send the complaint?"