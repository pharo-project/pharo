source
extractToTempTextSelection
	| aMethod |
	aMethod := self model selectedMethod.
	aMethod ifNil: [ ^ self model flashSourceCodeArea ].
	self model contentSelection asString ifEmpty: [ ^ self model flashSourceCodeArea ].
	self extractToTemporaryBetween: self model selectionInterval from: aMethod