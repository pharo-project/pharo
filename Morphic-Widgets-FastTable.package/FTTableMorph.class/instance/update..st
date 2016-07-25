updating
update: symbol
	symbol == #refresh ifTrue: [ ^ self refresh ].
	^ super update: symbol
	