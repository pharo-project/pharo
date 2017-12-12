Class {
	#name : #HEAdditionalMethodState,
	#superclass : #HEExportedLiteral,
	#instVars : [
		'method',
		'selector'
	],
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEAdditionalMethodState class >> tag [
	^ 1.
]

{ #category : #converting }
HEAdditionalMethodState >> asLiteralIn: env [
	| additionalMethodState |
	additionalMethodState := AdditionalMethodState new: value value size.
	additionalMethodState method: env newMethod.
	additionalMethodState selector: (selector asLiteralIn: env).

	value value withIndexDo: [ :e :idx | additionalMethodState basicAt: idx put: (e asLiteralIn: env) ].
	^ additionalMethodState
]

{ #category : #reading }
HEAdditionalMethodState >> doReadFrom: aReader [
	method := HEExportedLiteral readFrom: aReader.
	selector := HEExportedLiteral readFrom: aReader.
	value := HEExportedLiteral readFrom: aReader.

]

{ #category : #accessing }
HEAdditionalMethodState >> value: anAdditionalMethodState [
	method := anAdditionalMethodState method asExportedLiteral.
	selector := anAdditionalMethodState selector asExportedLiteral.

	value := Array new: anAdditionalMethodState size.
	1 to: anAdditionalMethodState size do: [ :idx | value at: idx put: (anAdditionalMethodState basicAt: idx) ].
	value := value asExportedLiteral
]
