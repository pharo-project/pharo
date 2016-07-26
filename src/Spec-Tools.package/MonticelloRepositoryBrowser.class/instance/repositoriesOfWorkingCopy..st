private
repositoriesOfWorkingCopy: aWorkingCopy
	
	^ aWorkingCopy isNil
		ifFalse: [ aWorkingCopy repositoryGroup repositories]
		ifTrue: [ self allRepositories]	