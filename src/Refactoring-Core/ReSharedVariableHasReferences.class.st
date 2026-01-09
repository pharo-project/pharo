"
I fail when a Shared Variable has no references. 
My candidates and violators are both shared variable names. 
I have a single candidate, and therefore a single violator.
"
Class {
	#name : 'ReSharedVariableHasReferences',
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

{ #category : 'accessing - private' }
ReSharedVariableHasReferences >> candidates [

	^ { sharedVariable }
]

{ #category : 'instance creation' }
ReSharedVariableHasReferences >> hierarchyOf: aRBClass referencesSharedVariable: variableName [

	aClass := aRBClass instanceSide.
	sharedVariable := variableName 
]

{ #category : 'initialization' }
ReSharedVariableHasReferences >> initialize [ 
	super initialize.
	violators := OrderedCollection new
]

{ #category : 'displaying' }
ReSharedVariableHasReferences >> violationMessageOn: aStream violators: theViolators [

	| verb |
	theViolators ifEmpty: [ ^ self ].
	verb := theViolators asArray = self violators asArray
		        ifTrue: [ ' is still' ]
		        ifFalse: [ ' is not' ].
	aStream
		nextPutAll: ' Variable ';
		nextPutAll: sharedVariable;
		nextPutAll: verb;
		nextPutAll: ' referenced'
]

{ #category : 'accessing' }
ReSharedVariableHasReferences >> violators [

	violators ifNotNil: [ ^ violators ].
	
	violators := self candidates.
	aClass withAllSubclasses do: [ :each |
			{ each. each classSide } 
				do: [ :cls | cls localMethods do: [ :meth | 
						(meth refersToVariable: sharedVariable) 
							ifTrue: [ 
								" The variable is referred from somewhere, therefore it is not a violator "
								violators := #() ] ] ] ].
	^ violators
]
