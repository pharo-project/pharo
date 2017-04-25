accessing
openBrowserIfLostChanges

	self browserIfLostChanges: [:browser | 
		browser viewEntryItems ifNotEmpty: [ browser open ] ]