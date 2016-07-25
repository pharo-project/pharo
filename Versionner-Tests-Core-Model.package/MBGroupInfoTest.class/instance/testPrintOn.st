tests
testPrintOn
	| str | 
	self shouldnt: [str := MBGroupInfo new printString] raise: Error.
	self assert: (str = 'nil ---').
	
