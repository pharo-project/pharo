requirements
basicCodeDirectory
	^ self subdirectory isEmptyOrNil 
		ifTrue: [ self repositoryDirectory ]
		ifFalse: [ self repositoryDirectory / self subdirectory  ].