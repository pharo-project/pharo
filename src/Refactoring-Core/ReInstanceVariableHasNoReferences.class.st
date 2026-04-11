Class {
	#name : 'ReInstanceVariableHasNoReferences',
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
ReInstanceVariableHasNoReferences >> check [

	^ self violators isEmpty
]

{ #category : 'accessing' }
ReInstanceVariableHasNoReferences >> errorString [ 

	^ ' Variable ', instanceVariable , ' is still referenced'
]

{ #category : 'instance creation' }
ReInstanceVariableHasNoReferences >> hierarchyOf: aRBClass referencesInstanceVariable: variableName [ 
	aClass := aRBClass.
	instanceVariable := variableName 
]

{ #category : 'initialization' }
ReInstanceVariableHasNoReferences >> initialize [ 
	super initialize.
	violators := OrderedCollection new
]

{ #category : 'displaying' }
ReInstanceVariableHasNoReferences >> violationMessageOn: aStream [

	violators ifEmpty: [ ^ self ].
	^ aStream
		  nextPutAll: 'The variable variable ';
		  nextPutAll: instanceVariable;
		  nextPutAll: 'is referenced in:';
		  nextPutAll: (violators collect: [ :m | m selector ]) asArray asString
]

{ #category : 'accessing' }
ReInstanceVariableHasNoReferences >> violators [

	violators := OrderedCollection new.
	aClass withAllSubclasses do: [ :each |
			| res |
			res := each whichMethodsReferToInstanceVariable: instanceVariable.

			res isNotEmpty ifTrue: [ violators addAll: res ] ].

	^ violators
]
