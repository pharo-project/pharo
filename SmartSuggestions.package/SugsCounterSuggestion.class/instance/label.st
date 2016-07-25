accessing
label
	^self selectedNodeHasExecutionCounter
		ifFalse: [ 'Add Counter' translated ]
		ifTrue: [ 'Remove Counter' translated ]
	