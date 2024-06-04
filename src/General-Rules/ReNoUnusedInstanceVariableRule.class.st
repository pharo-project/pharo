Class {
	#name : 'ReNoUnusedInstanceVariableRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'testing' }
ReNoUnusedInstanceVariableRule class >> checksClass [

	^ true
]

{ #category : 'accessing' }
ReNoUnusedInstanceVariableRule class >> group [

	^ 'Optimization'
]

{ #category : 'accessing' }
ReNoUnusedInstanceVariableRule class >> rationale [
	^ 'Classes should have instance variables that are actually used - instance variables without a reference should be removed.'
]

{ #category : 'accessing' }
ReNoUnusedInstanceVariableRule class >> ruleName [
	^ 'Unused instance variable'
]

{ #category : 'accessing' }
ReNoUnusedInstanceVariableRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'NoUnusedInstanceVariableRule'
]

{ #category : 'running' }
ReNoUnusedInstanceVariableRule >> check: aClass forCritiquesDo: aCriticBlock [

	aClass slots
		reject: [ :slot |
			slot isReferenced or: [
				slot definingClass pragmas anySatisfy: [ :pragma | pragma selector = #ignoreUnusedVariables: and: [ (pragma argumentAt: 1) includes: slot name ] ] ] ]
		thenDo: [ :slot | aCriticBlock cull: (self critiqueFor: aClass about: slot name) ]
]

{ #category : 'running' }
ReNoUnusedInstanceVariableRule >> critiqueFor: aClass about: aVarName [

	| crit |
	crit :=  ReRemoveInstanceVariableCritique
				         for: aClass
				         instanceVariable: aVarName
				         by: self .




	^ crit
]
