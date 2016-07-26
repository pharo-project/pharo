printing
changeString
	^ 'Remove <1s>>>#<2s>'
		expandMacrosWith: self displayClassName
		with: selector