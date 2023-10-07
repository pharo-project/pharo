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
			res := (each whichSelectorsReferToClassVariable: sharedVariable).
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
	aClass := aRBClass.
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
