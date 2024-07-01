Trait {
	#name : 'TRefactoringCommandSupport',
	#instVars : [
		'model'
	],
	#category : 'SystemCommands-RefactoringSupport',
	#package : 'SystemCommands-RefactoringSupport'
}

{ #category : 'actions' }
TRefactoringCommandSupport >> computeInvalidArgNamesForSelector: aSymbol [

	| invalidArgNames |
	invalidArgNames := Set new.
	(model allImplementorsOf: aSymbol) do: [ :e |
		| mth |
		mth := e realClass >> aSymbol.
		invalidArgNames
			addAll: mth argumentNames;
			addAll: mth origin instVarNames ].
	^ invalidArgNames
]

{ #category : 'initialization' }
TRefactoringCommandSupport >> initializeDefaultOptionsOf: refactoring [

	NautilusRefactoring new refactoringOptions: refactoring
]

{ #category : 'initialization' }
TRefactoringCommandSupport >> setUpModelFromContext: aContext [
	| browser scope |
	
	browser := aContext browser.
	scope := browser defaultNavigationScope.
	scope isCurrentImage ifTrue: [ scope := ScopesManager defaultScope ].
	model := RBNamespace onEnvironment: scope asRBEnvironment
]
