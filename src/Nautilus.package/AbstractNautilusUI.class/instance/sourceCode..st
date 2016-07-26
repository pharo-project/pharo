browser compatibility
sourceCode: aText 

	self okToChange
		ifTrue: [ 
			self updateSourceCodeAreaWith: aText.
			self giveFocusToSourceCodeArea ]