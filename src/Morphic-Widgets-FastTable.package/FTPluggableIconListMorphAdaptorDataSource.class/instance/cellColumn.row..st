accessing
cellColumn: column row: rowIndex
	| object text icon |
	object := self elementAt: rowIndex.
	
	icon := self getIconSelector ifNotNil: [ :selector | 
		self model perform: selector with: object ].
	text := column transform: (self elementWrapped: object).
	
	^ self createCellText: text icon: icon.