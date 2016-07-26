events
updateWhenPrevious

	|tmpPane|
	self notifyAndRemoveCurrentPane.
	self stepToUse decrementNumberOfDisplay.
	self stepToUse overBeginning 
		ifTrue: [
			
			self currentIndex: self currentIndex - 1].
	self stepToUse input: self entity.
	self notifyAndSetAsCurrentPane: (self stepsAndPanes at: self currentStep) last .
	self updateBrowserTitle.