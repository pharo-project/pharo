enumerating
headersDo: twoArgumentBlock
	self hasHeaders
		ifTrue: [ self headers headersDo: twoArgumentBlock ]