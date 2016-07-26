actions api
addStep: aStepModel

	self steps add: aStepModel.
	self notifyStepAdded: aStepModel