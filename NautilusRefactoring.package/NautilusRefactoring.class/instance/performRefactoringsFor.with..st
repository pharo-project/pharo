private
performRefactoringsFor: aSymbol with: anArgument
	[ self performRefactorings: (self perform: aSymbol with: anArgument) ]
		on: RefactoringAborted
		do: [ :ex | self inform: 'Refactoring aborted' ]