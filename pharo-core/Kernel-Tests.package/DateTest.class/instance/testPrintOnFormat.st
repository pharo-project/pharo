tests
testPrintOnFormat
	self assert: (String streamContents: [ :str | january23rd2004 printOn: str format: #(3 2 1 $* 2 2) ]) equals: '04*Jan*23'