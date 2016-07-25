exception signaling
doHandlingMessageNotUnderstood: aBlock
	"MNU should be trapped and resignaled as a match error in a few places in the matcher.
	This method factors out this dialect-dependent code to make porting easier."
	^ aBlock
		on: MessageNotUnderstood
		do: [:ex | RxParser signalMatchException: 'invalid predicate selector']