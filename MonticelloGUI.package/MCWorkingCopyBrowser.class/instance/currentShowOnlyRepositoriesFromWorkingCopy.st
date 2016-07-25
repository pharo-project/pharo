morphic ui
currentShowOnlyRepositoriesFromWorkingCopy
	^showOnlyRepositoriesFromWorkingCopy ifTrue: [ 2] 
	ifFalse: [ 1] 