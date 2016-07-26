instructions
addBlockReturnTopIfRequired
	"If the current sequence is empty this means that there was a returntop before 
	then since there is no more stmts we do not need a blockreturntop"
	
	"cant we optimize this away later? Then the frontend can always just add a return...."
	
	| predecessors |
	self flag: 'cant we do this automatically.... frontend always adds return, we ignore it if there is a ret method before?'.
	
	predecessors := ir predecessorsOf: currentSequence.
	(currentSequence isEmpty and: [(predecessors isEmpty) or: [((ir predecessorsOf: currentSequence) 
								anySatisfy: [:each | (each last isBlockReturnTop not) and: [(each last isReturn) 
												or: [(each size > 1) and: [(each at: each size -1) isReturn] ]]])] ])
							ifTrue: [self popScope]
							ifFalse: [self blockReturnTop ]