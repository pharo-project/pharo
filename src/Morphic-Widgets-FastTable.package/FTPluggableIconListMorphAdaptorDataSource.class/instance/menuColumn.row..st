accessing
menuColumn: column row: rowIndex
	| menu |
	
	self getMenuSelector ifNil: [ ^ nil ].
	
	menu := UIManager default newMenuIn: self table for: self model.
	self getMenuSelector numArgs = 1 ifTrue: [ 
		^ self model perform: self getMenuSelector with: menu ].
	^ self model 
		perform: self getMenuSelector
		with: menu
		with: false 
