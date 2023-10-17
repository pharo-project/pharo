Class {
	#name : 'RBRemoveParameterParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBRemoveParameterParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
			addCase: { #rbClass -> RBRemoveParameterTransformation};
		addCase: { #rbClass -> RBRemoveParameterRefactoring };
	
		yourself
]

{ #category : 'accessing' }
RBRemoveParameterParametrizedTest >> constructor [
	^ #removeParameter:in:selector:
]

{ #category : 'failure tests' }
RBRemoveParameterParametrizedTest >> testFailureNonExistantName [

	self shouldFail: (self createRefactoringWithArguments: {
				 'asdf'.
				 RBBasicLintRuleTestData.
				 #checkClass: }).
	self shouldFail: (self createRefactoringWithArguments: {
				 'aSmalllintContext'.
				 RBBasicLintRuleTestData.
				 #checkClass1: })
]

{ #category : 'failure tests' }
RBRemoveParameterParametrizedTest >> testFailurePrimitiveMethods [

	| refactoring |
	(model classNamed: #Object)
		compile: 'foo123: a <primitive: 100> ^#() primitiveFailed'
		classified: #( #accessing ).
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               'a'.
				               Object.
				               #foo123: }.
	self shouldFail: refactoring
]

{ #category : 'failure tests' }
RBRemoveParameterParametrizedTest >> testFailureReferenceArgument [

	| refactoring |
	(model classNamed: #Object)
		compile: 'methWith: arg1 ^ arg1 * 8'
		classified: #( #accessing ).
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               'arg1'.
				               Object.
				               #methWith: }.
	self shouldFail: refactoring
]

{ #category : 'tests' }
RBRemoveParameterParametrizedTest >> testRemoveParameter [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ 'anArg' . RBClassDataForRefactoringTest . ('rename' , 'ThisMethod:') asSymbol }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	
	self 
		assert: (class parseTreeForSelector: #renameThisMethod) 
		equals: (self parseMethod: 'renameThisMethod
								^self').
	self 
		assert: (class parseTreeForSelector: #callMethod) 
		equals: (self parseMethod: 'callMethod
								^(self renameThisMethod)').
	self deny: (class directlyDefinesMethod: ('rename' , 'ThisMethod:') asSymbol)
]
