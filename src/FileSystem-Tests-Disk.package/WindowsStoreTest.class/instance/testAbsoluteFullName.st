testing
testAbsoluteFullName
	| filesystem |
	filesystem := FileSystem store: WindowsStore new.

	"Check dropping trailing slash."	
	self assert: (filesystem referenceTo: 'C:\tmp\') fullName equals: 'C:\tmp'.

	"Check round-trip conversion from String to FileReference back to String again."
	#( 	'C:\'		'D:\'		'\'		'C:\tmp'		'D:\tmp'		'C:\tmp\xx'      ) 
		do: [ :pathString | self assert: (filesystem referenceTo: pathString) fullName equals: pathString ].
	
	
