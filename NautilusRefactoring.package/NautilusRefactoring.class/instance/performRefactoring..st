performing
performRefactoring: aRefactoring
	"Try to properly label aRefactoring and perform it or open the changes browser, depending on the preferences of the user."
	
	self performRefactorings: {aRefactoring}