Class {
	#name : 'ReSharedVariableHasReferencesInDefiningClass',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'aClass',
		'variable',
		'violators'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReSharedVariableHasReferencesInDefiningClass >> check [ 
    ^ self violators isNotEmpty
]

{ #category : 'initialization' }
ReSharedVariableHasReferencesInDefiningClass >> class: definingClass referencesToSharedVariable: aSharedVarName [ 

    aClass := definingClass.
    variable := aSharedVarName.
    violators := nil

]

{ #category : 'displaying' }
ReSharedVariableHasReferencesInDefiningClass >> violationMessageOn: aStream [

    self violators do: [ :method |
        aStream
            nextPutAll: method printString;
            nextPutAll: ' references shared variable ';
            nextPutAll: variable;
            nextPut: Character cr ]
]

{ #category : 'accessing' }
ReSharedVariableHasReferencesInDefiningClass >> violators [ 

	 | res |
    violators ifNil: [
    	violators := OrderedCollection new.
    	res := aClass whichMethodsReferToSharedVariable: variable.
    	res isNotEmpty ifTrue: [ violators addAll: res ].

    	res := aClass classSide whichMethodsReferToSharedVariable: variable.
    	res isNotEmpty ifTrue: [ violators addAll: res ].
	].

	^ violators
]
