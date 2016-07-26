testing
hasPreviousStep
	"Return true if there is previous step,
	meaning that I am not a first step in spotter,
	false otherwise"
	<return: #Boolean>
	
	^ self spotter hasSteps
		and: [ self spotter steps first ~= self ]