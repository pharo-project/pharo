accessing
label
	self hasUnacceptedEdits ifTrue: [ ^'Accept to enable breakpoints' ].

	^self selectedNodeHasBreakpoint
		ifFalse: [ 'Break' translated ]
		ifTrue: [ 'Remove breakpoint' translated ]