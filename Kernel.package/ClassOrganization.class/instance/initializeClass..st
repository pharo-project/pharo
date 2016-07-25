initialization
initializeClass: aClass
	self initialize.
	organizedClass := aClass.
	organizedClass selectors do: [ :each | 
		self classify: each under: Protocol unclassified ]