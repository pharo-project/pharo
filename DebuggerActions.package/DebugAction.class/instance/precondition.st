actions
precondition

	^ self needsValidation not or: [ self debugger okToChange ]