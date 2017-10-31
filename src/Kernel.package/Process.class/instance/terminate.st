changing process state
terminate 
	"Stop the process that the receiver represents forever.  Unwind to execute pending ensure:/ifCurtailed: blocks before terminating."

	| ctxt unwindBlock oldList |
	terminating := self isTerminating
		ifTrue: [ ^ ProcessAlreadyTerminating signal ]
		ifFalse: [ true ].
	self isActiveProcess 
		ifTrue: [
			ctxt := thisContext.
			[ ctxt := ctxt findNextUnwindContextUpTo: nil.
			ctxt isNil ] whileFalse: 
				[ (ctxt tempAt: 2) ifNil:
					[ ctxt tempAt: 2 put: nil.
					unwindBlock := ctxt tempAt: 1.
					thisContext terminateTo: ctxt.
					unwindBlock value ] ].
			thisContext terminateTo: nil.
			self suspend ]
		ifFalse: [
			"Always suspend the process first so it doesn't accidentally get woken up"
			oldList := self suspend.
			suspendedContext ifNotNil: [
				"Figure out if we are terminating the process while waiting inside special object like
				in Semaphore>>critical:. So if waiting object is interesting on this situation we will ask it to handle it. In case of Semaphore>>critical, Semaphore will pop the suspendedContext so that we leave the ensure: block inside	Semaphore>>critical: without signaling the semaphore.
				This methods allow to not be restricted only on Semaphore case."
				suspendedContext := oldList handleProcessTerminationOfWaitingContext: suspendedContext.

				"If we are terminating a process halfways through an unwind, try to complete that unwind block first."
				(suspendedContext findNextUnwindContextUpTo: nil) ifNotNil: [ :outer |
					(suspendedContext findContextSuchThat: [ :c | c closure == (outer tempAt: 1) ]) ifNotNil: [ :inner |
						"This is an unwind block currently under evaluation"
						suspendedContext runUntilErrorOrReturnFrom: inner ] ].		
			ctxt := self popTo: suspendedContext bottomContext.
			[ ctxt == suspendedContext bottomContext ] whileFalse: [
				"There was a problem during termination. Make the user aware of the problem
				but ensure that the current process will be properly terminated."
				| stackCopy |
				stackCopy := ctxt copyStack.
				[ UnwindError signalIn: stackCopy ] forkNamed: 'Unwind error during termination'.
				ctxt terminateTo: ctxt sender.
				ctxt := self popTo: suspendedContext bottomContext ].
			"The suspendedContext of process that has never run will have 
			a pc equal to the startpc. #isTerminate would thus answer false
			even though the process is dead. To prevent that we set the pc
			of the suspendedContext to nil (#isDead ---> true)."
			(suspendedContext notNil and: [
				suspendedContext isBottomContext ]) ifTrue: [
					suspendedContext pc: nil ] ] ]