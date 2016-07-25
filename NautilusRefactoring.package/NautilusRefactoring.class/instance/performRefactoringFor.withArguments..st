private
performRefactoringFor: aSymbol withArguments: anArray
	[ self performRefactoring: (self perform: aSymbol withArguments: anArray) ]
		on: RefactoringAborted
		do: [ :ex | self inform: 'Refactoring aborted' ]