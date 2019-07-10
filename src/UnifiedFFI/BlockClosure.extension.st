Extension { #name : #BlockClosure }

{ #category : #'*UnifiedFFI' }
BlockClosure >> on: exception fork: handlerAction return: answerAction [
	"This is the same as #on:fork: but instead just fork and letting the flow continues, in 
	 case of an error it also evaluates answerAction and returns its result."
		
	^ self on: exception do: [:ex |
		| onDoCtx handler bottom thisCtx |
		
		onDoCtx := thisContext.
		thisCtx := onDoCtx home.

		"find the context on stack for which this method's is sender"

		[ onDoCtx sender == thisCtx] whileFalse: [ 
			onDoCtx := onDoCtx sender.
			onDoCtx ifNil: [ 
				"Can't find our home context. seems like we're already forked
				and handling another exception in new thread. In this case, just pass it through handler."
				^ handlerAction cull: ex ] ].

		bottom := [ Processor terminateActive ] asContext.
		onDoCtx privSender: bottom.

		handler := [ handlerAction cull: ex ] asContext.
		handler privSender: thisContext sender.

		(Process forContext: handler priority: Processor activePriority) resume.

		"cut the stack of current process"
		thisContext privSender: thisCtx.
		answerAction cull: exception ]

]
