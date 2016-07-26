source code area
selectedMethodSource
	self selectedMethod ifNil: [ ^ '' ].
	^ self selectedMethod sourceCode