accessing
read
	self currentScope
		keysAndValuesDo: [:key :value | value == true ifTrue: [read add: key]].
	^read