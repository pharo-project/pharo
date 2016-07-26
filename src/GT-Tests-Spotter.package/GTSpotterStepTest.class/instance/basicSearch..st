private
basicSearch: aString
	
	" assert conditions before search "
	self flag: '#currentStep should be made more stable. This will fail for an empty, uninitialized spotter / but after multiple diveIn/diveOut it will be ok - inconsistent'.
	self assert: spotter currentStep == spotter currentStep.
	
	" backup preconditions "
	previousStep := spotter currentStep.
	previousContext := previousStep context.

	self shouldnt: [ spotter setText: aString from: nil ] raise: Error.
	
	" backup postconditions "
	currentStep := spotter currentStep.
	currentContext := spotter currentStep context.
	
	" assert conditions after search "
	self assert: previousStep == currentStep.