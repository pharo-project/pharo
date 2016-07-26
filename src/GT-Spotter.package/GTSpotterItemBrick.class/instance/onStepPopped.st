event handling
onStepPopped

	
	(self stepModel spotter steps last = self stepModel) & self isChecked ifTrue: [
		self notifySelected ]