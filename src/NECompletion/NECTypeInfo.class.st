"
I'm used in ECInstVarTypeGuesser to store found type informations.
"
Class {
	#name : #NECTypeInfo,
	#superclass : #Object,
	#instVars : [
		'type',
		'kind',
		'temporaryOffset'
	],
	#category : 'NECompletion-Model'
}

{ #category : #'instance creation' }
NECTypeInfo class >> definedByLiteral: aClass [ 
	| newInstance |
	newInstance := self new.
	newInstance setType: aClass kind: 1.
	^ newInstance
]

{ #category : #'instance creation' }
NECTypeInfo class >> definedByMessageSend: aClass [ 
	| newInstance |
	newInstance := self new.
	newInstance setType: aClass kind: 2.
	^ newInstance
]

{ #category : #'instance creation' }
NECTypeInfo class >> definedByTemporaryVar: anInteger [
	| newInstance |
	newInstance := self new.
	newInstance setType: nil kind: 3.
	newInstance temporaryOffset: anInteger.
	^ newInstance
]

{ #category : #testing }
NECTypeInfo >> isDefinedByMessageSend [
	^kind == 2
]

{ #category : #testing }
NECTypeInfo >> isDefinedByTemporary [
	^ kind == 3
]

{ #category : #accessing }
NECTypeInfo >> priority [
	^kind
]

{ #category : #accessing }
NECTypeInfo >> setType: aClass kind: anInteger [ 
	type := aClass.
	kind := anInteger
]

{ #category : #accessing }
NECTypeInfo >> temporaryOffset [
	^temporaryOffset
]

{ #category : #accessing }
NECTypeInfo >> temporaryOffset: anInteger [ 
	temporaryOffset := anInteger
]

{ #category : #accessing }
NECTypeInfo >> type [
	^type
]

{ #category : #accessing }
NECTypeInfo >> type: aClass [ 
	type := aClass
]
