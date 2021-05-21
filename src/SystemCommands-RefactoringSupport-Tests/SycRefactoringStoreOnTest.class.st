Class {
	#name : #SycRefactoringStoreOnTest,
	#superclass : #TestCase,
	#category : #'SystemCommands-RefactoringSupport-Tests'
}

{ #category : #tests }
SycRefactoringStoreOnTest >> testRemoveMessageArgumentCommandIsProperlyStoreOn [

	| refactorings |
	refactorings := SycRemoveMessageArgumentCommand new
		originalMessage: (MockTreeTableItem >> #title:) ast;
		argumentName: 'anObject';
		asRefactorings.
	
	self 
		assert: (String streamContents: [ :s | refactorings storeOn: s ])
		equals: '((Array new: 1) at: 1 put: (RBRemoveParameterRefactoring removeParameter: ''anObject'' in: MockTreeTableItem selector: #title:); yourself)'
]

{ #category : #tests }
SycRefactoringStoreOnTest >> testSSycRenameMessageCommandIsProperlyStoreOn [

	| refactorings node |
	
	node := (MockTreeTableItem >> #title:) ast.
	
	refactorings := SycRenameMessageCommand new
		originalMessage: (SycMessageDescription ofMessageNode: node);
		newSignature: (RBMethodName selector: #title: arguments: #('anObject')) ;
		asRefactorings.
	
	self 
		assert: (String streamContents: [ :s | refactorings storeOn: s ])
		equals: '((Array new: 1) at: 1 put: (RBRenameMethodRefactoring renameMethod: #title: in: MockTreeTableItem to: #title: permutation: (1 to: 1)); yourself)'
]

{ #category : #tests }
SycRefactoringStoreOnTest >> testSycAddMessageArgumentCommandIsProperlyStoreOn [

	| refactorings node newSelector |
	
	node := (MockTreeTableItem >> #title:) ast.
	newSelector := (RBMethodName selector: #title:lala: arguments: #(#anObject #anObject1))
		newArgs: (Array with: (RBArgumentName name: 'anObject1' value: 'nil'));
		permutation: #(1 -1).
	refactorings := SycAddMessageArgumentCommand new
		originalMessage: (SycMessageDescription ofMessageNode: node);
		newSelector: newSelector;
		asRefactorings.
	
	self 
		assert: (String streamContents: [ :s | refactorings storeOn: s ])
		equals: '((Array new: 1) at: 1 put: (RBAddParameterRefactoring addParameterToMethod: #title: in: MockTreeTableItem newSelector: #title:lala: permutation: #(1 -1) newArgs: ''((Array new: 1) at: 1 put: ; yourself)''); yourself)'
]
