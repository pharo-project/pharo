testing
isEnabled
	^ self model assisted
		ifTrue: [ self text isEmptyOrNil not ]
		ifFalse: [ self listIndex > 0 ]