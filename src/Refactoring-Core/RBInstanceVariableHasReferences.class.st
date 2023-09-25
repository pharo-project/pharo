Class {
	#name : 'RBInstanceVariableHasReferences',
	#superclass : 'RBCondition',
	#instVars : [
		'violators',
		'aClass',
		'instanceVariable'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBInstanceVariableHasReferences >> check [
	
	aClass withAllSubclasses do: [ :each |
			| res |
			res := (each whichSelectorsReferToInstanceVariable: instanceVariable).
			res isNotEmpty
				ifTrue: [ violators addAll: res ].
			].
	^ violators isNotEmpty
]

{ #category : 'instance creation' }
RBInstanceVariableHasReferences >> hierarchyOf: aRBClass referencesInstanceVariable: variableName [ 
	aClass := aRBClass.
	instanceVariable := variableName 
]

{ #category : 'initialization' }
RBInstanceVariableHasReferences >> initialize [ 
	super initialize.
	violators := OrderedCollection new
]
