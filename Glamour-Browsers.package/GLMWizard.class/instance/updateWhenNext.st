events
updateWhenNext

	|tmpPane|
	self populateInputOfStep: self stepToUse.  
	self stepToUse incrementNumberOfDisplay.
	self stepToUse overEnd
		ifTrue: [
			self currentIndex: self currentIndex + 1.
			self populateInputOfStep: self stepToUse.  
			].
	
	tmpPane := self computePane.
	self notifyAndAddNewPane: self computePane.
	