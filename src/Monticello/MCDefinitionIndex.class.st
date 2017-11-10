"
I'm a simple container of MCDefinitions which can be added or removed.
"
Class {
	#name : #MCDefinitionIndex,
	#superclass : #Object,
	#instVars : [
		'definitions'
	],
	#category : #Monticello-Patching
}

{ #category : #instance-creation }
MCDefinitionIndex class >> definitions: aCollection [
	^ self new addAll: aCollection
]

{ #category : #adding }
MCDefinitionIndex >> add: aDefinition [
	^ definitions at: aDefinition description put: aDefinition
]

{ #category : #adding }
MCDefinitionIndex >> addAll: aCollection [
	aCollection do: [:ea | self add: ea]
]

{ #category : #accessing }
MCDefinitionIndex >> definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock [
	| definition |
	definition := definitions at: aDefinition description ifAbsent: [].
	^ definition
		ifNil: errorBlock
		ifNotNil: [foundBlock value: definition]
]

{ #category : #accessing }
MCDefinitionIndex >> definitions [
	^ definitions values
]

{ #category : #initialization }
MCDefinitionIndex >> initialize [
	super initialize.
	definitions := Dictionary new
]

{ #category : #removing }
MCDefinitionIndex >> remove: aDefinition [
	definitions removeKey: aDefinition description ifAbsent: []
]
