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
		(UIManager default confirm: 'Do you want select another scope?') ifTrue: [ |scopes choosenScope|
			scopes := browser allNavigationScopes.
			choosenScope := UIManager default
				chooseFrom: ( scopes collect: #description)
				values: scopes
				title: 'Choose the scope'.
			choosenScope ifNil: [ 
				self inform: 'Command aborted, you must select a scope.'.
			CmdCommandAborted signal ].
		scope := choosenScope.
		]
	].
	model := RBClassModelFactory rbNamespace onEnvironment: scope asRBEnvironment
]
