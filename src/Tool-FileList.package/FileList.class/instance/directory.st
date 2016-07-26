accessing
directory
	
	^ reference isFile 
		ifTrue: [ reference parent ]
		ifFalse: [ reference ]