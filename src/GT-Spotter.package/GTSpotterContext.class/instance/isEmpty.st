testing
isEmpty
	^ self text isNil
		or: [ self textTrimmed isEmpty ]