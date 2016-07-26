class initialization
initialize 
	
	ErrorRecursion := false.

	"Ungly check to make sure the debugger will work until everything is in the image"
	(Smalltalk tools respondsTo: #registerDebugger:withRank:) ifFalse: [ ^ self ].

	"Add ourselves to registry. See [Smalltalk tools]" 
	Smalltalk tools registerDebugger: self withRank: 2.