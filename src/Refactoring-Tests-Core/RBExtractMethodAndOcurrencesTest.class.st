Class {
	#name : #RBExtractMethodAndOcurrencesTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBExtractMethodAndOcurrencesTest >> setUp [ 
	super setUp.
	model := self extractMethodTestData.
]

{ #category : #tests }
RBExtractMethodAndOcurrencesTest >> testBadInterval [
	|class|
	class := model classNamed: #MyClassA.
	self
		shouldFail:
			(RBExtractMethodAndOcurrences 
				model: model
				extract: (127 to: 136)
				from: #myMethod
				in:  class)
]

{ #category : #tests }
RBExtractMethodAndOcurrencesTest >> testExample [
	|class refactoring|"
	class := model classNamed: #MyClassA."
	refactoring := (RBExtractMethodAndOcurrences
				model: model 
				extract: (282 to: 302)
				from: #myMethod
				in:  (model classNamed: #MyClassA)).
	self setupMethodNameFor: refactoring toReturn: #exapleee.
	self halt.
	self executeRefactoring: refactoring .
	self halt.
	model changes
]
