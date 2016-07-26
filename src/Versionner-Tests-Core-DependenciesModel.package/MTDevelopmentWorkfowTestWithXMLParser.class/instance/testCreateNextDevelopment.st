tests
testCreateNextDevelopment
	project devWorkflow createNextDevelopment: '999'.
	self assert: (project configurationClass selectors includes: #'baseline999:').