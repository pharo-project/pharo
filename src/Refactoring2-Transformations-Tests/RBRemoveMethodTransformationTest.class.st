Class {
	#name : #RBRemoveMethodTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBRemoveMethodTransformationTest >> testMethodIsReferenced [

	self shouldFail: (RBRemoveMethodTransformation
							selector: #checkClass:
							from: #RBBasicLintRuleTestData)
							asRefactoring 
]

{ #category : #testing }
RBRemoveMethodTransformationTest >> testMethodSendsSuper [
	<expectedFailure>
	self shouldFail: (RBRemoveMethodTransformation
							selector: #new
							from: RBBasicLintRuleTestData class)
							asRefactoring
]

{ #category : #testing }
RBRemoveMethodTransformationTest >> testRecursiveMethod [

	| refactoring class selector otherClass |
	class := model classNamed: #Object.
	selector := 'someMethodThatIsNotReferenced' asSymbol.
	class compile: 'someMethodThatIsNotReferenced ^2' classified: #(#accessing).
	self assert: (class definesMethod: selector).
	
	otherClass := model metaclassNamed: self class superclass name.
	otherClass 
		compile: 'someMethodThatIsNotReferenced ^3 someMethodThatIsNotReferenced'
		classified: #(#accessing).
	model removeClassNamed: self class name.
	
	refactoring := (RBRemoveMethodTransformation
				model: model
				selector: selector
				from: class)
				asRefactoring.
	[ refactoring transform ] on: RBRefactoringError do: [ :e | e resume ].
	self deny: (class definesMethod: selector)
]

{ #category : #testing }
RBRemoveMethodTransformationTest >> testRefactoring [

	| refactoring selector class |
	selector := 'selectorNotReferenced' asSymbol.
	refactoring := (RBRemoveMethodTransformation
						selector: selector
						from: #RBRefactoryTestDataApp)
						asRefactoring.
	
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class directlyDefinesMethod: selector).
	refactoring transform.
	self deny: (class directlyDefinesMethod: selector)
]

{ #category : #testing }
RBRemoveMethodTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveMethodTransformation 
		selector: #one
		from: self changeMock name)
		transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self deny: (class directlyDefinesMethod: #one).
]
