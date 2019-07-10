Class {
	#name : #RBAbstractClassVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBAbstractClassVariableTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAbstractClassVariableTest >> testAbstractClassVariable [
	| refactoring meta class |
	refactoring := RBAbstractClassVariableRefactoring 
		variable: 'RecursiveSelfRule'
		class: RBTransformationRuleTestData.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	meta := class classSide.
	self assert: (meta parseTreeFor: #recursiveSelfRule) equals: (self parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self assert: (meta parseTreeFor: #recursiveSelfRule:) equals: (self parseMethod: 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').
	self assert: (meta parseTreeFor: #nuke) equals: (self parseMethod: 'nuke
							self recursiveSelfRule: nil').
	self assert: (meta parseTreeFor: #initializeAfterLoad1) equals: (self parseMethod: 'initializeAfterLoad1
							self recursiveSelfRule: RBParseTreeSearcher new.
							self recursiveSelfRule
								addMethodSearches: #(''`@methodName: `@args | `@temps | self `@methodName: `@args'' ''`@methodName: `@args | `@temps | ^self `@methodName: `@args'')
										-> [:aNode :answer | true]').
	self assert: (class parseTreeFor: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext 
							class := aSmalllintContext selectedClass.
							(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
									[(self class recursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
										ifFalse: 
											[builder compile: rewriteRule tree printString
												in: class
												classified: aSmalllintContext protocols]]')
]

{ #category : #'failure tests' }
RBAbstractClassVariableTest >> testInheritedName [
	self shouldFail: (RBAbstractClassVariableRefactoring 
			variable: #DependentsFields
			class: RBBasicLintRuleTestData)
]

{ #category : #'failure tests' }
RBAbstractClassVariableTest >> testMetaClassFailure [
	self shouldFail: (RBAbstractClassVariableRefactoring 
			variable: #RecursiveSelfRule
			class: RBTransformationRuleTestData class)
]

{ #category : #tests }
RBAbstractClassVariableTest >> testModelAbstractClassVariable [
	| refactoring meta class |
	class := model classNamed: #Foo.
	meta := class classSide.
	refactoring := RBAbstractClassVariableRefactoring 
		model: model
		variable: 'ClassVarName1'
		class: class.
	self executeRefactoring: refactoring.
	self assert: (meta parseTreeFor: #classVarName1) equals: (self parseMethod: 'classVarName1 ^ClassVarName1').
	self assert: (meta parseTreeFor: #classVarName1:) equals: (self parseMethod: 'classVarName1: anObject ^ClassVarName1 := anObject').
	self assert: (meta parseTreeFor: #foo) equals: (self parseMethod: 'foo
					^self classVarName1: self classVarName1 * self classVarName1 * self classVarName1').
	self assert: (class parseTreeFor: #classVarName1) equals: (self parseMethod: 'classVarName1
							^self class classVarName1').
	self assert: (class parseTreeFor: #classVarName1:) equals: (self parseMethod: 'classVarName1: anObject
							^self class classVarName1: anObject').
	self assert: (class parseTreeFor: #asdf) equals: (self parseMethod: 'asdf
						^self classVarName1: (self class classVarName1: self class classVarName1 + 1)').
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo) equals: (self parseMethod: 'foo
					instVarName1 := instVarName1 + instVarName2 + self class classVarName1').
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo) equals: (self parseMethod: 'foo
						instVarName1 := instVarName1 + instVarName2 + self class classVarName1')
]

{ #category : #tests }
RBAbstractClassVariableTest >> testModelAbstractClassVariableOverridenMethodsInSubclass [
	| refactoring meta class |
	class := model classNamed: #Foo.
	meta := class classSide.
	refactoring := RBAbstractClassVariableRefactoring 
		model: model
		variable: 'ClassVarName2'
		class: class.
	self executeRefactoring: refactoring.
	self assert: (meta parseTreeFor: #classVarName21) equals: (self parseMethod: 'classVarName21 ^ClassVarName2').
	self assert: (meta parseTreeFor: #classVarName21:) equals: (self parseMethod: 'classVarName21: anObject ClassVarName2 := anObject')
]

{ #category : #'failure tests' }
RBAbstractClassVariableTest >> testNonExistantName [
	self shouldFail: (RBAbstractClassVariableRefactoring 
			variable: #Foo
			class: RBBasicLintRuleTestData)
]
