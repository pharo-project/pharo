update cycle
displayWorldSafely: aWorld
	"Update this world's display and keep track of errors during draw methods."

	[aWorld displayWorld] ifError: [:err :rcvr |
		"Handle a drawing error"
		| errCtx errMorph |
		errCtx := thisContext.
		[
			errCtx := errCtx sender.
			"Search the sender chain to find the morph causing the problem"
			[errCtx notNil and:[(errCtx receiver isMorph) not]] 
				whileTrue:[errCtx := errCtx sender].
			"If we're at the root of the context chain then we have a fatal drawing problem"
			errCtx ifNil:[^self handleFatalDrawingError: err].
			errMorph := errCtx receiver.
			"If the morph causing the problem has already the #drawError flag set,
			then search for the next morph above in the caller chain."
			errMorph hasProperty: #errorOnDraw
		] whileTrue.
		errMorph setProperty: #errorOnDraw toValue: true.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	].