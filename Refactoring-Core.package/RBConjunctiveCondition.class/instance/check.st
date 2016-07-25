checking
check
	left check
		ifFalse: 
			[failed := #leftFailed.
			^false].
	right check
		ifFalse: 
			[failed := #rightFailed.
			^false].
	^true