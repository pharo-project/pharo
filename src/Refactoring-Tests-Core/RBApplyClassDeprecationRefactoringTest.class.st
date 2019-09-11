Class {
	#name : #RBApplyClassDeprecationRefactoringTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBApplyClassDeprecationRefactoringTest >> testBadName [
	self
		shouldFail: (RBApplyClassDeprecationRefactoring 
				rename: RBLintRuleTestData
				to: self objectClassVariable);
		shouldFail: (RBApplyClassDeprecationRefactoring 
				rename: RBLintRuleTestData
				to: #'Ob ject')
]

{ #category : #tests }
RBApplyClassDeprecationRefactoringTest >> testMetaClassFailure [
	self 
		shouldFail: (RBApplyClassDeprecationRefactoring rename: self class class to: #Foo)
]

{ #category : #tests }
RBApplyClassDeprecationRefactoringTest >> testModelRenameClass [
	| refactoring replacement deprecated user |

	model defineClass: 'Object subclass: #ClassDeprecatorTestingClassReplacement instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.

	model defineClass: 'ClassDeprecatorTestingClassReplacement subclass: #ClassDeprecatorTestingClassDeprecated instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	
	model defineClass: 'ClassDeprecatorTestingClassDeprecated subclass: #ClassDeprecatorTestingClassDeprecatedUser instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.

	replacement := model classNamed: #ClassDeprecatorTestingClassReplacement.
	deprecated := model classNamed: #ClassDeprecatorTestingClassDeprecated.
	user := model classNamed: #ClassDeprecatorTestingClassDeprecatedUser.

	user
		compile: 'foo	^ClassDeprecatorTestingClassDeprecated' classified: #(#accessing);
		compile: 'objectName ^#(ClassDeprecatorTestingClassDeprecated)' classified: #(#accessing).
	refactoring := RBApplyClassDeprecationRefactoring 
		model: model
		rename: deprecated
		to: #ClassDeprecatorTestingClassReplacement.
	
	"because of RB limitations we cannot detect extensions in the test properly so we skip this operation"
	refactoring shouldCopyExtensions: false.	
	refactoring shouldRemoveExtensions: false.	
		
	self executeRefactoring: refactoring.
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassReplacement).
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassDeprecated).
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassDeprecatedUser).

	self assert: (model classNamed: #ClassDeprecatorTestingClassReplacement) equals: replacement.
	self assert: (model classNamed: #ClassDeprecatorTestingClassDeprecated) equals: deprecated.
	self assert: (model classNamed: #ClassDeprecatorTestingClassDeprecatedUser) equals: user.

	self assert: (user parseTreeFor: #foo)
		equals: (self parseMethod: 'foo ^ClassDeprecatorTestingClassReplacement').
		
	self assert: (user parseTreeFor: #objectName) 
		equals: (self parseMethod: 'objectName ^#(ClassDeprecatorTestingClassReplacement)').

	self assert: user superclass name 
		equals: #ClassDeprecatorTestingClassReplacement
]
