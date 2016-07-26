accessing
nameMenuColumn: column row: rowIndex
	| method menu |
	method := (self elementAt: rowIndex) method.
	menu := UIManager default newMenuIn: self table for: self.
	menu add: ('Browse ', (method methodClass name), '>>#', method selector) 
		target: method
		selector: #browse.
	menu add: ('Inspect #', (method methodClass name), '>>#', method selector)
		target: method
		selector: #inspect.

	^ menu