private
performRefactoringFor: aSymbol with: anArgument
	[ self performRefactoring: (self perform: aSymbol with: anArgument) ]
		on: RefactoringAborted
		do: [ :ex | self inform: 'Refactoring aborted' ]