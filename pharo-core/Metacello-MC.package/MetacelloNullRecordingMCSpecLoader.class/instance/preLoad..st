doits
preLoad: packageOrVersionSpec

	self evalDoits ifFalse: [ ^self ].
	packageOrVersionSpec preLoadDoItBlock ~~ nil
		ifTrue: [ self beforeLoads add: packageOrVersionSpec name, ' load' ].
	super preLoad: packageOrVersionSpec