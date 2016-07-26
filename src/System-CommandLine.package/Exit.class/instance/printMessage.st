accessing
printMessage
	|stderr|
	stderr := VTermOutputDriver stderr.
	self isSuccess ifFalse: [ stderr red ].
	stderr
		nextPutAll: self messageText; lf;
		clear.