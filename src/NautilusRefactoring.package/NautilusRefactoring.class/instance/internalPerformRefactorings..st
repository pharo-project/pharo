performing
internalPerformRefactorings: aCollectionRefactoring
	"Try to properly label aRefactoring and perform it or open the changes browser, depending on the preferences of the user."

	self promptOnRefactoring
		ifFalse: [ 
			aCollectionRefactoring
				do: [ :e | 
					self refactoringOptions: e.
					e execute ] ]
		ifTrue: [ 
			aCollectionRefactoring
				do: [ :e | 
					self refactoringOptions: e.
					e primitiveExecute ].
			aCollectionRefactoring ifEmpty: [ ^ self ].
			(ChangesBrowser changes: aCollectionRefactoring) open ]