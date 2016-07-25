printing
changeString
	^ 'Add instance variable <1s> to <2s>'
		expandMacrosWith: self variable
		with: self displayClassName