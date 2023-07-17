Trait {
	#name : #TRefactoringCommandSupport,
	#instVars : [
		'model'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #actions }
TRefactoringCommandSupport >> computeInvalidArgNamesForSelector: aSymbol [

	| invalidArgNames |
	invalidArgNames := Set new.
	(model allImplementorsOf: aSymbol) do: [ :e |
		| method |
		method := e realClass >> aSymbol.
		invalidArgNames
			addAll: method argumentNames;
			addAll: method origin instVarNames ].
	^ invalidArgNames
]

{ #category : #initialization }
TRefactoringCommandSupport >> initializeDefaultOptionsOf: refactoring [

	NautilusRefactoring new refactoringOptions: refactoring
]

{ #category : #initialization }
TRefactoringCommandSupport >> setUpModelFromContext: aContext [
	| browser scope |
	browser := aContext browser.
	scope := browser defaultNavigationScope.
	scope isCurrentImage ifTrue: [ scope := RefactoringSettings scopeRefactoring ].
	model := RBClassModelFactory rbNamespace onEnvironment: scope asRBEnvironment
]
