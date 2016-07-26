source
extractToMethodTextSelection
	| aMethod |
	aMethod := self model selectedMethod.
	aMethod ifNil: [ ^ self model flashSourceCodeArea ].
	self model contentSelection asString ifEmpty: [ ^ self model flashSourceCodeArea ].
	self extractBetween: self model selectionInterval from: aMethod