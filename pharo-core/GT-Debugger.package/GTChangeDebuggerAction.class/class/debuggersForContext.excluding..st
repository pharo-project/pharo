accessing
debuggersForContext: aContext excluding: aDebugger 
	"Ungly check to make sure the debugger will work until everything is in the image"
	(Smalltalk tools respondsTo: #debuggersForContext:) 
		ifTrue: [ ^ (Smalltalk tools debuggersForContext: aContext) reverse
			remove: aDebugger class ifAbsent: [ ];
			yourself]
		ifFalse: [ ^ OrderedCollection new ]