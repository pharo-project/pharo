accessing - dynamic
previousStep
	"Return a spotter step before me which must exist,
	otherwise behavior is undefined."
	<return: #GTSpotterStep>
	self assert: self hasPreviousStep.
	
	^ self spotter steps at: 
			(self spotter steps indexOf: self) - 1