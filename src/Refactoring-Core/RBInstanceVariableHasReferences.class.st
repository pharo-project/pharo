Class {
	#name : 'RBInstanceVariableHasReferences',
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
RBInstanceVariableHasReferences >> check [
	
	aClass withAllSubclasses do: [ :each |
			| res |
			res := (each whichMethodsReferToInstanceVariable: instanceVariable).
			res isNotEmpty
				ifTrue: [ violators addAll: res ].
			].
	^ violators isNotEmpty
]

{ #category : 'accessing' }
RBInstanceVariableHasReferences >> errorString [ 

	^ ' Variable ', instanceVariable , ' is still referenced'
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

{ #category : 'accessing' }
RBInstanceVariableHasReferences >> violators [ 
	^ violators
]
