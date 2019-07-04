Class {
	#name : #RBProtectInstanceVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBProtectInstanceVariableTest >> testProtectInstanceVariable [
	| refactoring class |
	refactoring := RBProtectInstanceVariableRefactoring 
		variable: 'rewrite' , 'Rule1'
		class: RBSubclassOfClassToRename.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBSubclassOfClassToRename.
	self assert: (class parseTreeFor: #calls1) equals: (self parseMethod: 'calls1
								^rewriteRule1 := (rewriteRule1 := self calls)').
	self assert: (class parseTreeFor: #calls) equals: (self parseMethod: 'calls
								^rewriteRule1 := rewriteRule1 , rewriteRule1').
	self deny: (class directlyDefinesMethod: ('rewrite' , 'Rule1') asSymbol).
	self deny: (class directlyDefinesMethod: ('rewrite' , 'Rule1:') asSymbol)
]

{ #category : #'failure tests' }
RBProtectInstanceVariableTest >> testVariableNotDefined [
	| refactoring |
	refactoring := RBProtectInstanceVariableRefactoring 
		variable: 'rewrite'
		class: RBSubclassOfClassToRename.
	self shouldFail: refactoring
]
