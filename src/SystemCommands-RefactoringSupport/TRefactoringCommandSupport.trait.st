Trait {
	#name : #TRefactoringCommandSupport,
	#instVars : [
		'model'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #initialization }
TRefactoringCommandSupport >> initializeDefaultOptionsOf: refactoring [

	NautilusRefactoring new refactoringOptions: refactoring
]

{ #category : #initialization }
TRefactoringCommandSupport >> setUpModelFromContext: aContext [
	| browser scope |
	browser := aContext browser.
	scope := browser defaultNavigationScope.
	scope description = 'current image' ifTrue: [ 
		| scopes choosenScope |
		scopes := browser allNavigationScopes.
		choosenScope := scopes select: [ :e | e isKindOf: RefactoringSettings scopeRefactoring ].
		choosenScope ifNotNil: [ scope := choosenScope first ]
	].
	model := RBClassModelFactory rbNamespace onEnvironment: scope asRBEnvironment
]
