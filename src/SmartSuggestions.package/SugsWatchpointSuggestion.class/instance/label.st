accessing
label
	^self selectedNodeHasWatchpoint
		ifFalse: [ 'Add Watchpoint' translated ]
		ifTrue: [ 'Remove Watchpoint' translated ]
	