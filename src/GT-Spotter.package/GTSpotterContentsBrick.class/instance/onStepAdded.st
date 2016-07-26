events-processing
onStepAdded
	"as soon as step added I need to show drop down pane"
	self isDropDownPaneVisible ifTrue: [ ^ self ].
	
	self showDropDownPane