Class {
	#name : 'ReVariableHasReferencesTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Model',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Model'
}

{ #category : 'accessing' }
ReVariableHasReferencesTest >> classToTest [

	self subclassResponsibility 
]

{ #category : 'accessing' }
ReVariableHasReferencesTest >> model [

	^ model ifNil: [
			  model := RBNamespace onEnvironment: (RBClassEnvironment classes: {
						            MyClassAlpha.
						            MyClassBeta.
						            self class }) ]
]
