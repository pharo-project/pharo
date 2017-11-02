Extension { #name : #MessageBrowser }

{ #category : #'*MessageBrowserRefactoringAddition' }
MessageBrowser >> browsedEnvironment [
	^ browsedEnvironment ifNil: [ browsedEnvironment := RBBrowserEnvironment new ]
]

{ #category : #'*MessageBrowserRefactoringAddition' }
MessageBrowser class >> methodRefactoringSubMenu: aBuilder [
	<messageBrowserListMenu>
	| target |
	target := aBuilder model.
	target selectedMethod ifNil: [ ^ target ].
	aBuilder model selectedClass methodRefactoring: aBuilder
]

{ #category : #'*MessageBrowserRefactoringAddition' }
MessageBrowser >> refactor [
	^ NautilusRefactoring model: self
]

{ #category : #'*MessageBrowserRefactoringAddition' }
MessageBrowser class >> refactoringMethodMenu: aBuilder [
	<messageBrowserListMenu>
	| target |
	target := aBuilder model.
	target selectedMethod ifNil: [ ^ target ].
	aBuilder model selectedClass refactoringMethod: aBuilder
]
