tests - printing
testPrintSeparatedByEverySignedOn
	| printer |
	printer := [ :integer :printSigned | 
		String streamContents: [ :stream | 
			integer
				printSeparatedBy: $.
				every: 3
				signed: printSigned
				on: stream ] ].
	self assert: (printer value: 123456789 value: false) equals: '123.456.789'.
	self assert: (printer value: -123456789 value: false) equals: '-123.456.789'.
	self assert: (printer value: 123456789 value: true) equals: '+123.456.789'.
	self assert: (printer value: -123456789 value: true) equals: '-123.456.789'