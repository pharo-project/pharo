Class {
	#name : 'ReProtectInstanceVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
ReProtectInstanceVariableParametrizedTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  addCase: {
				  (#rbClass -> ReProtectInstanceVariableRefactoring).
				  (#constructor -> #variable:class:) };
		  yourself
]

{ #category : 'running' }
ReProtectInstanceVariableParametrizedTest >> setUp [

	super setUp.
	model := self modelOnClasses: {
			         RBSubclassOfClassToRename.
			         RBBasicLintRuleTestData.
			         RBSmallSubclassOfSubclassOfClassToRename.
			         RBSiblingOfSmallSubclass }
]

{ #category : 'failure tests' }
ReProtectInstanceVariableParametrizedTest >> testFailureVariableNotDefined [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { 'rewrite'. #RBSubclassOfClassToRename }).
	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { 'foo'. #RBBasicLintRuleTestData })
]

{ #category : 'tests' }
ReProtectInstanceVariableParametrizedTest >> testProtectInstanceVariable [
	| refactoring class |
	refactoring := self createRefactoringWithModel: model andArguments:
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

{ #category : 'tests' }
ReProtectInstanceVariableParametrizedTest >> testProtectInstanceVariableWithAccessorFromTheSuperclass [

	| refactoring sibling |
	refactoring := self createRefactoringWithModel: model andArguments: {
			               ('rewrite' , 'Rule1').
			               #RBSubclassOfClassToRename }.
	self executeRefactoring: refactoring.
	sibling := refactoring model classNamed: #RBSiblingOfSmallSubclass.
	self assert: (sibling parseTreeForSelector: #use2) equals: (self parseMethod: 'use2 rewriteRule1:= 13')
]

{ #category : 'tests' }
ReProtectInstanceVariableParametrizedTest >> testProtectInstanceVariableWithAccessorInSubclass [

	| refactoring subclass |
	refactoring := self createRefactoringWithModel: model andArguments: {
			               ('rewrite' , 'Rule1').
			               #RBSubclassOfClassToRename }.
	self executeRefactoring: refactoring.
	subclass := refactoring model classNamed: #RBSmallSubclassOfSubclassOfClassToRename.
	self assert: (subclass parseTreeForSelector: #useAccessor) equals: (self parseMethod: 'useAccessor ^rewriteRule1:= 54')
]
