Class {
	#name : #RBDeprecateClassTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #tests }
RBDeprecateClassTest >> testBadName [
	self
		shouldFail: (RBDeprecateClassRefactoring 
				deprecated: #RBLintRuleTestData
				in: #'Ob ject')
]

{ #category : #tests }
RBDeprecateClassTest >> testMetaClassFailure [
	self 
		shouldFail: (RBDeprecateClassRefactoring deprecated: self class class name in: #Foo)
]

{ #category : #tests }
RBDeprecateClassTest >> testModelRenameClass [
	| refactoring replacement deprecated user newSubclass |

	model defineClass: 'Object subclass: #ClassDeprecatorTestingClassReplacement instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.

	model defineClass: 'ClassDeprecatorTestingClassReplacement subclass: #ClassDeprecatorTestingClassDeprecated instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	
	model defineClass: 'ClassDeprecatorTestingClassDeprecated subclass: #ClassDeprecatorTestingClassDeprecatedUser instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.

	replacement := model classNamed: #ClassDeprecatorTestingClassReplacement.
	deprecated := model classNamed: #ClassDeprecatorTestingClassDeprecated.
	user := model classNamed: #ClassDeprecatorTestingClassDeprecatedUser.

	user
		compile: 'foo	^ClassDeprecatorTestingClassDeprecated' classified: #(#accessing);
		compile: 'objectName ^#(ClassDeprecatorTestingClassDeprecated)' classified: #(#accessing).
	refactoring := RBDeprecateClassRefactoring 
		model: model
		deprecated: #ClassDeprecatorTestingClassDeprecated
		in: #ClassDeprecatorTestingClassReplacement.
		
	self executeRefactoring: refactoring.
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassReplacement).
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassDeprecated).
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassDeprecated1).
	self assert: (model includesClassNamed: #ClassDeprecatorTestingClassDeprecatedUser).

	self assert: (user parseTreeFor: #foo)
		equals: (self parseMethod: 'foo ^ClassDeprecatorTestingClassReplacement').
		
	self assert: (user parseTreeFor: #objectName) 
		equals: (self parseMethod: 'objectName ^#(ClassDeprecatorTestingClassReplacement)').

	self assert: user superclass name 
		equals: #ClassDeprecatorTestingClassDeprecated1.
		
	newSubclass := model classNamed: #ClassDeprecatorTestingClassDeprecated1.
	self assert: (newSubclass classSide parseTreeFor: #isDeprecated)
		equals: (self parseMethod: 'isDeprecated ^true').
]
