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
		newSignature: (SycMessageDescription ofMessageNode: node) ;
		asRefactorings.
	
	self 
		assert: (String streamContents: [ :s | refactorings storeOn: s ])
		equals: '((Array new: 1) at: 1 put: (RBRenameMethodRefactoring renameMethod: #title: in: MockTreeTableItem to: #title: permutation: ((OrderedCollection new) add: 1; yourself)); yourself)'
]

{ #category : #tests }
SycRefactoringStoreOnTest >> testSycAddMessageArgumentCommandIsProperlyStoreOn [

	| refactorings |
	refactorings := SycAddMessageArgumentCommand new
		originalMessage: (MockTreeTableItem >> #title:) ast;
		newSelector: (SycMessageDescription new
			selector: #title:lala:;
			argumentNames: #(#anObject #anObject1));
		argumentDefaultValue: 'nil';
		asRefactorings.
	
	self 
		assert: (String streamContents: [ :s | refactorings storeOn: s ])
		equals: '((Array new: 1) at: 1 put: (RBAddParameterRefactoring addParameterToMethod: #title: in: MockTreeTableItem newSelector: #title:lala: permutation: #(1 0) initializer: ''nil''); yourself)'
]
