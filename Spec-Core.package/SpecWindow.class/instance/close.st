controls
close

	self model askOkToClose
		ifTrue: [ self okToChange ifFalse: [ ^ self ] ].
		
	super close