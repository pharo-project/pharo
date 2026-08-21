Class {
	#name : 'ReNewNegatedConditionTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReNewNegatedConditionTest >> model [

	^ model ifNil: [
		  model := RBNamespace onEnvironment: (RBClassEnvironment classes: { Object }).
		  model
			  defineClass: [ :aBuilder |
				  aBuilder
					  superclass: Object;
					  name: #ReNewNegatedConditionConcrete;
					  package: #'Refactory-Test data' ];
			  defineClass: [ :aBuilder |
				  aBuilder
					  superclass: Object;
					  name: #ReNewNegatedConditionReference;
					  package: #'Refactory-Test data' ];
			  defineClass: [ :aBuilder |
				  aBuilder
					  superclass: Object;
					  name: #ReNewNegatedConditionAbstract;
					  package: #'Refactory-Test data' ].
		  (model classNamed: #ReNewNegatedConditionReference)
			  compile: 'reference ^ ReNewNegatedConditionConcrete'
			  classified: #testing.
		  (model classNamed: #ReNewNegatedConditionAbstract)
			  compile: 'required self subclassResponsibility'
			  classified: #testing.
		  model ]
]

{ #category : 'tests' }
ReNewNegatedConditionTest >> testNegatedFalseConditionViolators [

	| cls notcond condition |
	cls := self model classNamed: #ReNewNegatedConditionConcrete.
	condition := ReClassesAreAbstractCondition new classes: { cls }.
	notcond := condition not.

	self
		deny: condition check;
		assert: notcond check;
		assert: condition violators equals: { cls };
		assert: notcond violators isEmpty ;
		assert: condition nonViolators isEmpty;
		assert: notcond nonViolators equals: { cls }
]

{ #category : 'tests' }
ReNewNegatedConditionTest >> testNegatedTrueConditionViolators [

	| cls notcond condition |
	cls := self model classNamed: #ReNewNegatedConditionAbstract.
	condition := ReClassesAreAbstractCondition new classes: { cls }.
	notcond := condition not.

	self
		assert: condition check;
		deny: notcond check;
		assert: condition violators isEmpty;
		assert: notcond violators equals: { cls };
		assert: condition nonViolators equals: { cls };
		assert: notcond nonViolators isEmpty
]
