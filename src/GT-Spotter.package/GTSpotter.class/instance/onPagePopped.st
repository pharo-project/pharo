event handling
onPagePopped
	
	self steps size > 0 ifTrue: [
		self currentStep: self steps last ]