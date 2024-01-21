Class {
	#name : 'ReInstanceVariableHasReferences',
	#superclass : 'RBNewAbstractCondition',
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
ReInstanceVariableHasReferences >> check [
	
	aClass withAllSubclasses do: [ :each |
			| res |
			res := (each whichMethodsReferToInstanceVariable: instanceVariable).
			res isNotEmpty
				ifTrue: [ violators addAll: res ].
			].
	^ violators isNotEmpty
]

{ #category : 'accessing' }
ReInstanceVariableHasReferences >> errorString [ 

	^ ' Variable ', instanceVariable , ' is still referenced'
]

{ #category : 'instance creation' }
ReInstanceVariableHasReferences >> hierarchyOf: aRBClass referencesInstanceVariable: variableName [ 
	aClass := aRBClass.
	instanceVariable := variableName 
]

{ #category : 'initialization' }
ReInstanceVariableHasReferences >> initialize [ 
	super initialize.
	violators := OrderedCollection new
]

{ #category : 'accessing' }
ReInstanceVariableHasReferences >> violators [ 
	^ violators
]
