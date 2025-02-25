Class {
	#name : 'RBProtectInstanceVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBProtectInstanceVariableParametrizedTest class >> testParameters [
self flag: 'need change tranformation to have the same behavior of ref'.
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBProtectInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
	"	addCase: { #rbClass -> RBProtectVariableTransformation .
					  #constructor -> #instanceVariable:class: };
	"	yourself
]

{ #category : 'running' }
RBProtectInstanceVariableParametrizedTest >> setUp [

	super setUp.
	model := self modelOnClasses: { RBSubclassOfClassToRename . RBBasicLintRuleTestData }
]

{ #category : 'failure tests' }
RBProtectInstanceVariableParametrizedTest >> testFailureVariableNotDefined [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { 'rewrite'. #RBSubclassOfClassToRename }).
	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { 'foo'. #RBBasicLintRuleTestData })
]

{ #category : 'tests' }
RBProtectInstanceVariableParametrizedTest >> testProtectInstanceVariable [
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
