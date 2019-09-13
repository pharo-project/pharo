"
A variable definition
"
Class {
	#name : #RGVariable,
	#superclass : #RGElement,
	#category : #'Ring-Core-Kernel'
}

{ #category : #accessing }
RGVariable >> definitionString [
	"non special globals are defined by the symbol"
	^self isSpecial
		ifTrue: [ String streamContents: [ :aStream |
			aStream nextPutAll: '#'; nextPutAll: self name]]
		ifFalse: [self name printString]

]

{ #category : #testing }
RGVariable >> isClassInstanceVariable [

	^false
]

{ #category : #testing }
RGVariable >> isClassVariable [

	^false
]

{ #category : #testing }
RGVariable >> isPoolVariable [

	^false
]

{ #category : #testing }
RGVariable >> isSpecial [

	^ false
]

{ #category : #testing }
RGVariable >> isVariableDefinition [

	^ true
]

{ #category : #accessing }
RGVariable >> name: aString [

	self parent announceDefinitionChangeDuring: [ 
		super name: aString ].

]
