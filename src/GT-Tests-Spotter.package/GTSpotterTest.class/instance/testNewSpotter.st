tests
testNewSpotter
	| step1 |
	self testInstanceCreation.
	
	self assert: spotter notNil.
	self assert: spotter hasSteps not.
	self assert: spotter steps isEmpty.
	self deny: spotter hasMultipleSteps.
	
	self shouldnt: [ step1 := spotter currentStep ] raise: Error.
	
	self flag: '#currentStep is not always cached in spotter - especially when it is a new/empty spotter. In that case #currentStep will always return a new instance of a step. The implementation would be more polymorphic, oo-style, simpler and with much less ifTrue/ifFalse if the initial-state were regarded as a step as well (as an inst-var not in the inst-var-list of #steps). Maybe use a separate type to distinguish? GTSpotterInitialStep ... '.
	"self assert: step1 == spotter currentStep"
	
	self assert: step1 notNil.
	self assert: spotter hasSteps not.
	self assert: spotter steps isEmpty.