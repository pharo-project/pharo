Class {
	#name : 'ReClassesConditionTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassesConditionTest >> model [

	^ model ifNil: [
			  model := RBNamespace onEnvironment: (RBClassEnvironment classes: {
						            MyClassAlpha.
						            MyClassBeta.
						            MyClassBetaSibling.
						            MyClassARoot.
						            MySubAccessingSuperclassState.
						            MyClassB.
						            Object.
						            Number }) ]
]
