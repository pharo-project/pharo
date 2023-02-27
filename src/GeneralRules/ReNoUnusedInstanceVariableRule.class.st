Class {
	#name : #ReNoUnusedInstanceVariableRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReNoUnusedInstanceVariableRule class >> checksClass [

	^ true
]

{ #category : #accessing }
ReNoUnusedInstanceVariableRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'NoUnusedInstanceVariableRule'
]

{ #category : #running }
ReNoUnusedInstanceVariableRule >> check: aClass forCritiquesDo: aCriticBlock [
	aClass slots
		select: [ :slot | slot isReferenced not ]
		thenDo: [ :slot | aCriticBlock cull: (self critiqueFor: aClass about: slot name) ]
]

{ #category : #running }
ReNoUnusedInstanceVariableRule >> critiqueFor: aClass about: aVarName [

	| crit |
	crit :=  ReRemoveInstanceVariableCritique
				         for: aClass
				         instanceVariable: aVarName
				         by: self .




	^ crit
]

{ #category : #accessing }
ReNoUnusedInstanceVariableRule >> group [

	^ 'Optimization'
]

{ #category : #accessing }
ReNoUnusedInstanceVariableRule >> name [

	^ 'Unused instance variable'
]

{ #category : #accessing }
ReNoUnusedInstanceVariableRule >> rationale [
	^ 'Classes should have instance variables that are actually used - instance variables without a reference should be removed.'
]
