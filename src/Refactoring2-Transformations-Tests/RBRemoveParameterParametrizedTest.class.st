Class {
	#name : #RBRemoveParameterParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRemoveParameterParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRemoveParameterRefactoring };
		yourself
]

{ #category : #'failure tests' }
RBRemoveParameterParametrizedTest >> testNonExistantName [
	self
		shouldFail: (rbClass 
				removeParameter: 'asdf'
				in: RBBasicLintRuleTestData
				selector: #checkClass:);
		shouldFail: (rbClass 
				removeParameter: 'aSmalllintContext'
				in: RBBasicLintRuleTestData
				selector: #checkClass1:)
]

{ #category : #'failure tests' }
RBRemoveParameterParametrizedTest >> testPrimitiveMethods [
	| refactoring |
	(model classNamed: #Object) 
		compile: 'foo123: a <primitive: 100> ^#() primitiveFailed'
		classified: #(#accessing).
	refactoring := rbClass 
				model: model
				removeParameter: 'a'
				in: Object
				selector: #foo123:.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBRemoveParameterParametrizedTest >> testReferenceArgument [
	| refactoring |
	(model classNamed: #Object) 
		compile: 'methWith: arg1 ^ arg1 * 8'
		classified: #(#accessing).
	refactoring := rbClass 
				model: model
				removeParameter: 'arg1'
				in: Object
				selector: #methWith:.
	self shouldFail: refactoring
]

{ #category : #tests }
RBRemoveParameterParametrizedTest >> testRemoveParameter [
	| refactoring class |
	refactoring := rbClass 
		removeParameter: 'anArg'
		in: RBRefactoryTestDataApp
		selector: ('rename' , 'ThisMethod:') asSymbol.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #renameThisMethod) equals: (self parseMethod: 'renameThisMethod
								^self').
	self assert: (class parseTreeFor: #callMethod) equals: (self parseMethod: 'callMethod
								^(self renameThisMethod)').
	self deny: (class directlyDefinesMethod: ('rename' , 'ThisMethod:') asSymbol)
]
