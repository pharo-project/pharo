visiting
visitBlockNode: aBlockNode
	| savedWritten |
	"If we're in the optimized block in one side of an optimized ifTrue:ifFalse: et al
	 leave it to the enclosing visitMessageNode: activation to handle merging written."
	inOptimizedBlock ifTrue:
		[^super visitBlockNode: aBlockNode].
	"If we're not then don't update written because without evaluating the guard(s)
	 we can't tell if the block is evaluated or not, and we must avoid false positives."
	savedWritten := written copy.
	super visitBlockNode: aBlockNode.
	written := savedWritten