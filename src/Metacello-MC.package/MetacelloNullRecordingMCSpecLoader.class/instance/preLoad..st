doits
preLoad: packageOrVersionSpec

	self evalDoits ifFalse: [ ^self ].
	packageOrVersionSpec preLoadDoItBlock ~~ nil
		ifTrue: [ self beforeLoads add: packageOrVersionSpec printString, ' load' ].
	super preLoad: packageOrVersionSpec