testing
testPrintString
	| filesystem |
	filesystem := FileSystem store: WindowsStore new.
	
	"path"
	self assert: (filesystem referenceTo: 'a/b') printString equals: 'File @ a\b'.
	self assert: (filesystem referenceTo: '\test.txt') printString equals: 'File @ \test.txt'.
	
	"root"
	self assert: (filesystem referenceTo: '/') printString equals: 'File @ \'.
	self assert: (filesystem referenceTo: '\') printString equals: 'File @ \'.
	
	"drive root"
	self assert: (filesystem referenceTo: 'c:\') printString equals: 'File @ c:\'.
	
	"empty"
	self assert: (filesystem referenceTo: './') printString equals: 'File @ .'.
	self assert: (filesystem referenceTo: '.\') printString equals: 'File @ .'