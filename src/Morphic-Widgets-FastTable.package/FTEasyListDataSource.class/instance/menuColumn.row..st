accessing
menuColumn: column row: rowIndex
	menuBlock ifNil: [ ^ nil ].
	rowIndex = 0 ifTrue: [ 
		"I could answer a menu without selection, but in this case I will just answer nil, 
		 which means 'no menu'"
		^ nil ].
	^ menuBlock value: (self elementAt: rowIndex)