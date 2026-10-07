Class {
	#name : 'RBMoveMethodToClassParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBMoveMethodToClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> ReMoveMethodToClassRefactoring };
		addCase: { #rbClass -> RBMoveMethodToClassTransformation };
		yourself
]

{ #category : 'accessing' }
RBMoveMethodToClassParametrizedTest >> constructor [
	^ #model:method:class:
]

{ #category : 'failure tests' }
RBMoveMethodToClassParametrizedTest >> testFailureMethodAlreadyDefined [

	| method targetClass sourceClass |
	sourceClass := model classNamed: #RBTransformationRuleTestData1.
	targetClass := model classNamed: #RBFooLintRuleTestData.
	method := (sourceClass methodFor: #foo) compiledMethod.
	"refactoring will raise warning, so we proceed and test if the change was performed in the model"
	self proceedThroughWarning: [
		(self createRefactoringWithArguments: { model . method . targetClass }) generateChanges].
	self deny: (sourceClass definesMethod: #foo).
	self assert: (targetClass methodFor: #foo) source equals: 'foo' , String cr , String tab , '^ foo'
	
]
