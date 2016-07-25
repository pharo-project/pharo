event handling
showContextMenu
	| menu |
	menu := MenuMorph new defaultTarget: self.
	
	edited 
		ifTrue: [
			menu add: 'accept' selector: #accept.
			menu add: 'clear' selector: #clear.
			menu add: 'cancel' selector: #cancel.
			].
		
	menu popUpInWorld