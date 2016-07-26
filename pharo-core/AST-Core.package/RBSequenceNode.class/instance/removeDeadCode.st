replacing
removeDeadCode
	(self isUsed ifTrue: [statements size - 1] ifFalse: [statements size]) 
		to: 1
		by: -1
		do: 
			[:i | 
			(statements at: i) isImmediateNode 
				ifTrue: 
					[self clearReplacements.
					statements removeAt: i]].
	super removeDeadCode