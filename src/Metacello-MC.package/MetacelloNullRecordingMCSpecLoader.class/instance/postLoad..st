doits
postLoad: packageOrVersionSpec

	self evalDoits ifFalse: [ ^self ].
	packageOrVersionSpec postLoadDoItBlock ~~ nil
		ifTrue: [ self afterLoads add: packageOrVersionSpec name, ' load' ].
	super postLoad: packageOrVersionSpec