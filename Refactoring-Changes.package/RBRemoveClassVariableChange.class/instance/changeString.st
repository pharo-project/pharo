printing
changeString
	^ 'Remove class variable <1s> from <2s>' 
		expandMacrosWith: self variable
		with: self displayClassName