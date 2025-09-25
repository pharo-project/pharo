"
I check if a Shared Variable is referenced from methods in aClass or any of its subclasses
"
Class {
	#name : 'ReSharedVariableHasReferences',
	#superclass : 'ReVariableCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReSharedVariableHasReferences >> check [

	aClass withAllSubclasses do: [ :each |
			| res |
			res := (each whichMethodsReferToSharedVariable: variable).
			res isNotEmpty
				ifTrue: [ violators addAll: res ].
			].
	aClass withAllSubclasses do: [ :each |
			| res |
			res := (each classSide whichMethodsReferToSharedVariable: variable).
			res isNotEmpty
				ifTrue: [ violators addAll: res ].
			].
	^ violators isNotEmpty
]

{ #category : 'accessing' }
ReSharedVariableHasReferences >> errorString [ 

	^ ' Variable ', variable , ' is still referenced'
]

{ #category : 'instance creation' }
ReSharedVariableHasReferences >> hierarchyOf: aRBClass referencesSharedVariable: variableName [

	aClass := aRBClass instanceSide.
	variable := variableName 
]

{ #category : 'initialization' }
ReSharedVariableHasReferences >> initialize [ 
	super initialize.
	violators := OrderedCollection new
]

{ #category : 'accessing' }
ReSharedVariableHasReferences >> violatorCandidates [

	^ aClass withAllSubclasses flatCollect: [ :cls | cls selectors , cls classSide selectors ]
]

{ #category : 'accessing' }
ReSharedVariableHasReferences >> violators [ 
	^ violators
]
