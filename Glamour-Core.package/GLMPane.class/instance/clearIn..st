accessing
clearIn: aContext 
	self notingPresentationChangeDo: [
		self presentations clear.
		self ports do: [:each | each value: nil in: aContext copy ]]