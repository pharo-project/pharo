accessing
menuColumn: column row: rowIndex
	rowIndex = 0 ifTrue: [ 
		"I could answer a menu without selection, but in this case I will just answer nil, 
		 which means 'no menu'"
		^ nil ].

	"protocols do not have associated menu"
	column id = 'Name' ifTrue: [ ^ self nameMenuColumn: column row: rowIndex ].
	column id = 'Origin' ifTrue: [ ^ self originMenuColumn: column row: rowIndex ].	

	^ nil	