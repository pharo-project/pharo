Class {
	#name : 'RBSharedVariableHasReferences',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'aClass',
		'violators',
		'sharedVariable'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
RBSharedVariableHasReferences >> check [
	
	aClass withAllSubclasses do: [ :each |
			| res |
			res := (each whichMethodsReferToSharedVariable: sharedVariable).
			res isNotEmpty
				ifTrue: [ violators addAll: res ].
			].
	aClass withAllSubclasses do: [ :each |
			| res |
			res := (each classSide whichMethodsReferToSharedVariable: sharedVariable).
			res isNotEmpty
				ifTrue: [ violators addAll: res ].
			].
	^ violators isNotEmpty
]

{ #category : 'accessing' }
RBSharedVariableHasReferences >> errorString [ 

	^ ' Variable ', sharedVariable , ' is still referenced'
]

{ #category : 'instance creation' }
RBSharedVariableHasReferences >> hierarchyOf: aRBClass referencesSharedVariable: variableName [

	aClass := aRBClass instanceSide.
	sharedVariable := variableName 
]

{ #category : 'initialization' }
RBSharedVariableHasReferences >> initialize [ 
	super initialize.
	violators := OrderedCollection new
]

{ #category : 'accessing' }
RBSharedVariableHasReferences >> violators [ 
	^ violators
]
