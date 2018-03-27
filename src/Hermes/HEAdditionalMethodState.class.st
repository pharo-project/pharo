"
I represent an AdditionMethodState.
I serialize myself into a stream with the following fields:

    - method
    - selector
    - an array containing all the elements in the original additional method state.

Each of this elements is exported as a HEExportedLiteral.
My value contains the array of elements in the AdditionalMethodState.
"
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
	"When the literal is deserialized in the environment, the method is the actual method that is currently deserialized (this #newMethod message)."
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
	"I extract the information from the additional method state and convert it to exported literals."
	method := anAdditionalMethodState method asExportedLiteral.
	selector := anAdditionalMethodState selector asExportedLiteral.

	value := Array new: anAdditionalMethodState size.
	1 to: anAdditionalMethodState size do: [ :idx | value at: idx put: (anAdditionalMethodState basicAt: idx) ].
	value := value asExportedLiteral
]
