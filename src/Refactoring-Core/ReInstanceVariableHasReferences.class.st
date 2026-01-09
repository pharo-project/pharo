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

{ #category : 'accessing - private' }
ReInstanceVariableHasReferences >> candidates [

	| candidates |
	candidates := #(  ) asSet.

	aClass withAllSubclasses do: [ :each | candidates addAll: (each newMethods collect: [ :meth | meth value ]) ].
	^ candidates
]

{ #category : 'displaying' }
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
	violators := #() asSet.
	 
	aClass withAllSubclasses do: [ :each |
		| res |
		res := (each whichMethodsReferToInstanceVariable: instanceVariable).
		res isEmpty
			ifTrue: [ violators addAll: res ].
		].
	^ violators
]
