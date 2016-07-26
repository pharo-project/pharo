tests - printing
testPrintOnBaseShowRadix
	self assert: (String streamContents: [ :str | 123 printOn: str base: 10 showRadix: false ]) equals: '123'.
	self assert: (String streamContents: [ :str | 123 printOn: str base: 10 showRadix: true ]) equals: '10r123'.
	self assert: (String streamContents: [ :str | 123 printOn: str base: 8 showRadix: false ]) equals: '173'.
	self assert: (String streamContents: [ :str | 123 printOn: str base: 8 showRadix: true ]) equals: '8r173'