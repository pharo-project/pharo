Class {
	#name : 'SycRefactoringStoreOnTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-RefactoringSupport-Tests',
	#package : 'SystemCommands-RefactoringSupport-Tests'
}

{ #category : 'tests' }
SycRefactoringStoreOnTest >> testRemoveMessageArgumentCommandIsProperlyStoreOn [

	| refactorings |
	refactorings := SycRemoveMessageArgumentCommand new
		originalMessage: (MockTreeTableItem >> #title:) ast;
		argumentName: 'anObject';
		refactoredClass: MockTreeTableItem;
		asRefactorings.

	self
		assert: (String streamContents: [ :s | refactorings first  storeOn: s ])
		equals: '(RBRemoveParameterRefactoring removeParameter: ''anObject'' in: MockTreeTableItem selector: #title:)'
]

{ #category : 'tests' }
SycRefactoringStoreOnTest >> testSSycRenameMessageCommandIsProperlyStoreOn2 [

	| refactorings node |

	node := (MockTreeTableItem >> #title:) ast.

	refactorings := SycRenameMessageCommand new
		originalMessage: (SycMessageDescription ofMessageNode: node);
		newSignature: (RBMethodName selector: #title: arguments: #('anObject')) ;
		refactoredClass: MockTreeTableItem;
		asRefactorings.

	self
		assert: (String streamContents: [ :s | refactorings storeOn: s ])
		equals: '((Array new: 1) at: 1 put: (RBRenameMethodRefactoring renameMethod: #title: in: MockTreeTableItem to: #title: permutation: (1 to: 1)); yourself)'
]

{ #category : 'tests' }
SycRefactoringStoreOnTest >> testSycAddMessageArgumentCommandIsProperlyStoreOn [

	| refactorings node newSelector |

	node := (MockTreeTableItem >> #title:) ast.
	newSelector := (RBMethodName selector: #title:lala: arguments: #(#anObject #anObject1))
		newArgs: (Array with: (RBArgumentName name: 'anObject1' value: 'nil'));
		permutation: #(1 -1).
	refactorings := SycAddMessageArgumentCommand new
		originalMessage: (SycMessageDescription ofMessageNode: node);
		refactoredClass: MockTreeTableItem;
		newSelector: newSelector;
		asRefactorings.

	self
		assert: (String streamContents: [ :s | refactorings storeOn: s ])
		equals: '((Array new: 1) at: 1 put: (RBAddParameterRefactoring addParameterToMethod: #title: in: MockTreeTableItem newSelector: #title:lala: permutation: #(1 -1) newArgs: ''((Array new: 1) at: 1 put: ; yourself)''); yourself)'
]
