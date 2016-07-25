modal control
modalSelection: anObject
	self setProperty: #modalSelection toValue: anObject.
	self isModalInvokationDone: true