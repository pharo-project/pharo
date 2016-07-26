helper
print: aLongErrorMesssage short: aShortErrorMesssage of: aTestCase 
	| testCaseString longMessage shortMessage offset cr |
	
	cr := OSPlatform current lineEnding.
	shortMessage := aShortErrorMesssage ifNil: [ '' ].
	longMessage := aLongErrorMesssage ifNil: [ '' ].
	
	testCaseString := aTestCase printString.
	self stdout 
		startOfLine; clearToEnd; nextPutAll: testCaseString.
	
	"Let's see if we have enough space to print the error"
	offset := 80 - testCaseString size - longMessage size.
	offset < 1
		ifTrue: [ 
			self stdout startOfLine; right: (80 - shortMessage size); 
				nextPutAll: shortMessage ]
		ifFalse: [ 
			self stdout right: offset;
				nextPutAll: longMessage ].
		
	self stdout clear.
	self stdout << cr