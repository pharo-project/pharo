Class {
	#name : #RBProtectInstanceVariableParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #tests }
RBProtectInstanceVariableParametrizedTest class >> testParameters [
self flag: 'need change tranformation to have the same behavior of ref'.
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBProtectInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
	"	addCase: { #rbClass -> RBProtectVariableTransformation .
					  #constructor -> #instanceVariable:class: };
	"	yourself
]

{ #category : #running }
RBProtectInstanceVariableParametrizedTest >> setUp [

	super setUp.
	model := self abstractVariableTestData
]

{ #category : #tests }
RBProtectInstanceVariableParametrizedTest >> testProtectInstanceVariable [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{('rewrite' , 'Rule1') . #RBSubclassOfClassToRename}.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBSubclassOfClassToRename.
	self assert: (class parseTreeForSelector: #calls1) equals: (self parseMethod: 'calls1
								^rewriteRule1 := (rewriteRule1 := self calls)').
	self assert: (class parseTreeForSelector: #calls) equals: (self parseMethod: 'calls
								^rewriteRule1 := rewriteRule1 , rewriteRule1').
	self deny: (class directlyDefinesMethod: ('rewrite' , 'Rule1') asSymbol).
	self deny: (class directlyDefinesMethod: ('rewrite' , 'Rule1:') asSymbol)
]

{ #category : #'failure tests' }
RBProtectInstanceVariableParametrizedTest >> testVariableNotDefined [
	self shouldFail:
		(self createRefactoringWithArguments: {'rewrite' . #RBSubclassOfClassToRename}).
	self shouldFail:
		(self createRefactoringWithArguments: {'foo' . #RBBasicLintRuleTestData})
]
