accessing
change
	"return changes provided by the refactoring"
	
	alreadyExecuted ifFalse: [
		refactoring primitiveExecute.
		alreadyExecuted := true ].
	
	^ refactoring changes