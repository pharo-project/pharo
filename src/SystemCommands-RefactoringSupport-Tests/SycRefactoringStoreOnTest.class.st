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
		class: MockTreeTableItem;
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
		originalMessage: (RBMethodName ofMessageNode: node);
		newSignature: (RBMethodName ofMessageNode: node);
		class: MockTreeTableItem;
		asRefactorings.
	
	self 
		assert: (String streamContents: [ :s | refactorings storeOn: s ])
		equals: '((Array new: 1) at: 1 put: (RBRenameMethodRefactoring renameMethod: #title: in: MockTreeTableItem to: #title: permutation: ((OrderedCollection new) add: 1; yourself)); yourself)'
]

{ #category : #tests }
SycRefactoringStoreOnTest >> testSycAddMessageArgumentCommandIsProperlyStoreOn [

	| refactorings |
	refactorings := SycAddMessageArgumentCommand new
		originalMessage: (RBMethodName ofMethod: (MockTreeTableItem >> #title:));
		newSelector: (RBMethodName new
			selector: #title:lala:;
			arguments: #(#anObject #anObject1));
		argumentDefaultValue: 'nil';
		class: MockTreeTableItem;
		asRefactorings.
	
	self 
		assert: (String streamContents: [ :s | refactorings storeOn: s ])
		equals: '((Array new: 1) at: 1 put: (RBAddParameterRefactoring addParameterToMethod: #title: in: MockTreeTableItem newSelector: #title:lala: permutation: #(1 0) initializer: ''nil''); yourself)'
]
