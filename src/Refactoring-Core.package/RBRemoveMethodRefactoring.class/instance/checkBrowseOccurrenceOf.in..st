preconditions
checkBrowseOccurrenceOf: selector in: aRBMethod
	self
		refactoringError:
			('Possible call to <2s> in <1p><n>Browse references?' expandMacrosWith: aRBMethod modelClass with: selector)
		with: [ self openBrowserOn: (RBBrowserEnvironment new referencesTo: selector) ]