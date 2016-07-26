accessing
dataSource: anObject
	dataSource := anObject.
	dataSource table: self.
	dataSource readyToBeDisplayed.
	self resetPosition.
	self refresh