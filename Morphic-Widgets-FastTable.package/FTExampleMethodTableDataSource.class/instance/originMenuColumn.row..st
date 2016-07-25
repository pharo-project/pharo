accessing
originMenuColumn: column row: rowIndex
	| method menu |
	method := (self elementAt: rowIndex) method.
	menu := UIManager default newMenuIn: self table for: self.
	menu add: ('Browse ', (method methodClass name)) 
		target: method methodClass
		selector: #browse.

	^ menu