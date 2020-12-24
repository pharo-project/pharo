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
				extract: (127 to: 136)
				from: #myMethod
				in:  class)
]

{ #category : #tests }
RBExtractMethodAndOcurrencesTest >> testExample [
	"|class refactoring|
	class := model classNamed: #MyClassA.
	refactoring := (RBExtractMethodAndOcurrences 
				extract: (282 to: 302)
				from: #myMethod
				in:  class).
				self setupMethodNameFor: refactoring toReturn: #exapleee.
				self executeRefactoring: refactoring .
				self halt.
				model changes"
]
