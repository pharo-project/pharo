actions
executeAction

	HelpBrowser open selectTopicSatisfying: [:each | 
		each owner notNil and:  [
			(each owner key = self debugger class helpClass key) and: [
				each title = 'Overview' ] ] ]