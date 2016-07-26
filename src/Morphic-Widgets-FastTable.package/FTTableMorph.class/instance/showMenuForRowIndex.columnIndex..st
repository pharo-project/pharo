menu
showMenuForRowIndex: rowIndex columnIndex: columnIndex
	| menu |

	(rowIndex notNil and: [ (self isRowIndexSelected: rowIndex) not ]) ifTrue: [ 
		self selectRowIndex: rowIndex ].

	menu := self dataSource 
		menuColumn: (columnIndex ifNotNil: [self columns at: columnIndex])
		row: (rowIndex ifNil: [0]).
		
	(menu isNil or: [ menu isInWorld ]) ifTrue: [ ^ self ].
	
	menu popUpInWorld: self currentWorld

	