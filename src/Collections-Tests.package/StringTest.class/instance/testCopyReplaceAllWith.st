tests - copy
testCopyReplaceAllWith
	self assert: ('--A--' copyReplaceAll: 'A' with: 'ABC') equals: '--ABC--'.
	self assert: ('--A--' copyReplaceAll: 'A' with: 'X') equals: '--X--'.
	self assert: ('--ABC--' copyReplaceAll: 'ABC' with: 'X') equals: '--X--'.
	self assert: ('--A--' copyReplaceAll: 'A' with: '') equals: '----'.
	self assert: ('' copyReplaceAll: 'A' with: 'ABC') equals: ''.
	self assert: ('A--A--A' copyReplaceAll: 'A' with: 'ABC') equals: 'ABC--ABC--ABC'.
	self assert: ('A--A--A' copyReplaceAll: 'A' with: 'X') equals: 'X--X--X'.
	self assert: ('ABC--ABC--ABC' copyReplaceAll: 'ABC' with: 'X') equals: 'X--X--X'.
	self assert: ('A--A--A' copyReplaceAll: 'A' with: '') equals: '----'.
	self assert: ('A--ABC--A' copyReplaceAll: 'ABC' with: 'X') equals: 'A--X--A'.
	self assert: ('AB--ABC--AB' copyReplaceAll: 'ABC' with: 'X') equals: 'AB--X--AB'.
	self assert: ('--A--' copyReplaceAll: '' with: '') equals: '--A--'.
	self assert: ('AAAA-' copyReplaceAll: 'AA' with: 'X') equals: 'XX-'