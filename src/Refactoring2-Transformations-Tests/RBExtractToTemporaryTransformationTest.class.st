Class {
	#name : #RBExtractToTemporaryTransformationTest,
	#superclass : #RBParametrizedTransformationTest,
	#instVars : [
		'methodSelector',
		'targetClass',
		'newVariableName',
		'interval',
		'newSource'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBExtractToTemporaryTransformationTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #targetClass -> #RBRefactoryTestDataApp. 
						#methodSelector ->  ('caller', '2') asSymbol. 
						#newVariableName -> 'temp'.
						#interval -> (52 to: 73).
						#newSource -> 'caller2
	^(1 to: 10) inject: 1 into: [:sum :each | | temp | temp := sum * (self foo: each). temp]' };
		addCase:  { #targetClass -> #RBRefactoryTestDataApp. 
						#methodSelector -> #noMoveDefinition. 
						#newVariableName ->  'asdf'.
						#interval -> (133 to: 141).
						#newSource -> 'noMoveDefinition
							| temp |
							^(self collect: 
								[:each | 
									temp := each printString.
									temp , temp]) 
								select: [:each | | asdf | asdf := each size. temp := asdf + temp]'};
		addCase:  { #targetClass -> #RBRefactoryTestDataApp. 
						#methodSelector -> #demoMethodWithDuplicates. 
						#newVariableName -> 'temp'.
						#interval -> (73 to: 77).
						#newSource ->  'demoMethodWithDuplicates
							| a b result1 result2 answer temp |
							a := 3.
							temp := a + 5.
							b := temp.
							result1 := a + b.
							result2 := temp.
							answer := result1 + result2 + temp.
							^ answer' };
		yourself
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> interval [

	^ interval
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> interval: anObject [

	interval := anObject
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> methodSelector [

	^ methodSelector
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> methodSelector: anObject [

	methodSelector := anObject
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> newSource [

	^ newSource
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> newSource: anObject [

	newSource := anObject
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> newVariableName [

	^ newVariableName
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> newVariableName: anObject [

	newVariableName := anObject
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> targetClass [

	^ targetClass
]

{ #category : #accessing }
RBExtractToTemporaryTransformationTest >> targetClass: anObject [

	targetClass := anObject
]

{ #category : #'failure tests' }
RBExtractToTemporaryTransformationTest >> testBadInterval [
	self
		shouldFail:
			(RBExtractToTemporaryTransformation
				extract: (24 to: 30)
				to: 'asdf'
				from: #testMethod
				in: RBRefactoryTestDataApp) asRefactoring;
		shouldFail:
			(RBExtractToTemporaryTransformation
				extract: (14 to: 105)
				to: 'asdf'
				from: #testMethod1
				in: RBRefactoryTestDataApp) asRefactoring;
		shouldFail:
			(RBExtractToTemporaryTransformation
				extract: (61 to: 101)
				to: 'asdf'
				from: #noMoveDefinition
				in: RBRefactoryTestDataApp) asRefactoring
]

{ #category : #'failure tests' }
RBExtractToTemporaryTransformationTest >> testBadName [
	self
		shouldFail:
			(RBExtractToTemporaryTransformation
				extract: (14 to: 23)
				to: 'a sdf'
				from: #testMethod
				in: RBRefactoryTestDataApp) asRefactoring
]

{ #category : #'failure tests' }
RBExtractToTemporaryTransformationTest >> testExtractToTemporaryRefactoring [
	| refactoring  |
	refactoring := RBExtractToTemporaryTransformation
		extract: interval
		to: newVariableName
		from: methodSelector
		in: targetClass.
	refactoring asRefactoring transform.

	self assert: ((refactoring model classNamed: targetClass) parseTreeFor: methodSelector) 
		equals: (self parseMethod: newSource)
]

{ #category : #'failure tests' }
RBExtractToTemporaryTransformationTest >> testExtractToTemporaryTransformation [
	| refactoring  |
	refactoring := RBExtractToTemporaryTransformation
		extract: interval
		to: newVariableName
		from: methodSelector
		in: targetClass.
	refactoring transform.

	self assert: ((refactoring model classNamed: targetClass) parseTreeFor: methodSelector) 
		equals: (self parseMethod: newSource)
]

{ #category : #'failure tests' }
RBExtractToTemporaryTransformationTest >> testNoValidNameOfTemp [
	self shouldFail: (RBExtractToTemporaryTransformation
			extract: (51 to: 60)
			to: 'collection'
			from: #modifiesCollection
			in: RBRefactoryTestDataApp) asRefactoring
]

{ #category : #'failure tests' }
RBExtractToTemporaryTransformationTest >> testNonExistantSelector [
	self shouldFail: (RBExtractToTemporaryTransformation
			extract: (14 to: 23)
			to: 'asdf'
			from: #checkClass1:
			in: RBRefactoryTestDataApp) asRefactoring
]
