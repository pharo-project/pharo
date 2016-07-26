accessing
currentStep
	^ currentStep
		ifNil: [ self hasSteps
				ifTrue: [ currentStep := self steps last ]
				ifFalse: [ self newStep origin: self foundationOrigin ] ]