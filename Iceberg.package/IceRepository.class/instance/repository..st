accessing
repository: anMCRepository
	repository := anMCRepository.
	
	self location exists
		ifTrue: [ 
			repository validate. 
			branch 
				ifNil: [ branch := repository currentBranch. ]
				ifNotNil: [ self checkoutBranch: branch ]]
		ifFalse: [ 
			self createLocalClone. 
			repository validate
		]
	