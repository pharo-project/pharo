"
A variable definition
"
Class {
	#name : #RG2VariableDefinition,
	#superclass : #RG2ElementDefinition,
	#category : #'Ring2-Core-Kernel'
}

{ #category : #testing }
RG2VariableDefinition >> isClassInstanceVariable [

	^false
]

{ #category : #testing }
RG2VariableDefinition >> isClassVariable [

	^false
]

{ #category : #testing }
RG2VariableDefinition >> isPoolVariable [

	^false
]

{ #category : #testing }
RG2VariableDefinition >> isSpecial [

	^ false
]

{ #category : #testing }
RG2VariableDefinition >> isVariableDefinition [

	^ true
]

{ #category : #'as yet unclassified' }
RG2VariableDefinition >> name: aString [

	self parent announceDefinitionChangeDuring: [ 
		super name: aString ].

]
