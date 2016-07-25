printing
changeString
	^ 'Add class variable <1s> to <2s>' 
		expandMacrosWith: self variable
		with: self displayClassName