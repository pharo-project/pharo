actions api
pushStep: stepModel
	
	self addStep: stepModel.
	"updating current step"
	self currentStep: stepModel