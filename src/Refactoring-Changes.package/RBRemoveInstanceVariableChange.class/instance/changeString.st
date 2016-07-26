printing
changeString
	^ 'Remove instance variable <1s> from <2s>' 
		expandMacrosWith: self variable
		with: self displayClassName