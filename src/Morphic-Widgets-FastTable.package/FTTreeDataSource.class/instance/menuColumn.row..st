accessing
menuColumn: column row: rowIndex
	rowIndex = 0
		ifTrue: [ ^ nil ].	"I could answer a menu without selection, but in this case I will just answer nil, which means 'no menu'"
			
	^ (UIManager default newMenuIn: self table for: self)
		add: 'Expand All ' target: self selector: #expandAll;
		add: 'Collapse All' target: self selector: #collapseAll;
		yourself