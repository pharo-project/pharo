accessing
hideColumnHeaders
	showColumnHeaders ifFalse: [ ^ self ].
	showColumnHeaders := false.
	self refresh