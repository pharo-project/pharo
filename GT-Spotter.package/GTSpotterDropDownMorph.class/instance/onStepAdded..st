event handling
onStepAdded: ann
	"as soon as step added we need to push it"

	self pushPane: (self buildPane: ann stepModel)