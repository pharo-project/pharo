building
terminateWith: aTerminator
	"Branch terminators are never supposed to change.
	Make sure this is the case."

	aTerminator ~~ self
		ifTrue: [RxParser signalCompilationException:
				'internal matcher build error - wrong terminator']