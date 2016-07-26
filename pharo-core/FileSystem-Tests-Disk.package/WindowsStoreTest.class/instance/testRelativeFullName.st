testing
testRelativeFullName

	"FIleSystem currently does not support MS Windows concept of each drive having its own working directory.
	That is too great a change during Pharo 3 Beta, so for now just drop drive from relative references. 
	Follow up in Case 13094"	

	| filesystem |
	filesystem := FileSystem store: WindowsStore new.
		
	self assert: (filesystem referenceTo: 'C:tmp') fullName equals: (filesystem referenceTo: 'tmp') fullName