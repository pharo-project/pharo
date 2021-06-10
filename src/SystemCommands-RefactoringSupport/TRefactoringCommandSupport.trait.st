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
	(model allImplementorsOf: aSymbol) do: [ :e | | cm | 
		cm:=(e realClass >> aSymbol).
		invalidArgNames addAll: cm argumentNames;
			addAll: cm origin instVarNames ].
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
	model := RBClassModelFactory rbNamespace onEnvironment: scope asRBEnvironment
]
