visitor
visitMonticelloVersionSave: aChange
	^ (self displayOperation: 'MC save:'), 
		(self displayString: aChange versionName asString), 
		(self displayOperation: ' on: '), 
		(self displayString: aChange repositoryDescription asString)