render thread
executeRenderOperations
	| operation  |
	[ operation := RenderThreadQueue nextOrNil.
	  operation  ~~ nil ] whileTrue: [
		operation execute.
	].
