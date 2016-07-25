utils
logDuring: aBlock
	
	Smalltalk logStdErrorDuring: [ :stderr | aBlock value: stderr ]

