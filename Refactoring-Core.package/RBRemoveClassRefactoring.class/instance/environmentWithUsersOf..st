preconditions
environmentWithUsersOf: aTrait
	^ RBClassEnvironment 
		onEnvironment: RBBrowserEnvironment new
		classes: aTrait realClass users