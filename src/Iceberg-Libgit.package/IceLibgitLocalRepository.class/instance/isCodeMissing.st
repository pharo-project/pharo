testing
isCodeMissing
	^ self subdirectory isEmptyOrNil 
		ifTrue: [ self isMissing ]
		ifFalse: [ (self repositoryDirectory / self subdirectory) exists not  ]