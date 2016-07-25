private
performRefactoringFor: aSymbol with: anArgument with: anotherArgument
	[ self performRefactoring: (self perform: aSymbol with: anArgument with: anotherArgument) ]
		on: RefactoringAborted
		do: [ :ex | self inform: 'Refactoring aborted' ]