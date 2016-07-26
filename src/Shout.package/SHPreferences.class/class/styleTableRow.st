settings
styleTableRow
	| allStyles |
	
	allStyles := Pragma  allNamed: #styleTable: in: SHTextStylerST80 class.
	^Smalltalk ui theme 
		newRowIn: World 
		for: (
			{ Smalltalk ui theme buttonLabelForText: 'Predefined styles: ' translated }, 
			(allStyles collect: [ :eachPragma | 
				(Smalltalk ui theme 
					newButtonIn: World 
						for:  self
						getState: nil 
						action: #setStyleTableNamed:  
						arguments:  { eachPragma argumentAt: 1 }
						getEnabled: nil 
						getLabel: nil
						help: ('Change style table to ', (eachPragma argumentAt: 1)) translated)
					label: (eachPragma argumentAt: 1);
					yourself ] ))