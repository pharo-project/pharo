Class {
	#name : #HESendMessage,
	#superclass : #HEExportedLiteral,
	#instVars : [
		'selector',
		'receiver',
		'arguments'
	],
	#category : #'Hermes-Model'
}

{ #category : #'instance creation' }
HESendMessage class >> selector: aSelector receiver: aReceiver arguments: arguments [

	^ self new
		selector: aSelector;
		receiver: aReceiver;
		arguments: arguments;
		yourself
]

{ #category : #'instance creation' }
HESendMessage class >> tag [
	^ 27
]

{ #category : #accessing }
HESendMessage >> arguments [
	^ arguments
]

{ #category : #accessing }
HESendMessage >> arguments: anObject [
	arguments := anObject
]

{ #category : #converting }
HESendMessage >> asLiteralIn: env [
	| aReceiver aSelector args |

	aReceiver := receiver asLiteralIn: env.
	aSelector := selector asLiteralIn: env.
	args := arguments asLiteralIn: env.
	
	^ aReceiver perform: aSelector withArguments: args
]

{ #category : #reading }
HESendMessage >> doReadFrom: aReader [ 

	selector := (HEExportedLiteral readFrom: aReader).
	receiver := (HEExportedLiteral readFrom: aReader).
	arguments := (HEExportedLiteral readFrom: aReader).
]

{ #category : #accessing }
HESendMessage >> receiver [
	^ receiver
]

{ #category : #accessing }
HESendMessage >> receiver: anObject [
	receiver := anObject
]

{ #category : #accessing }
HESendMessage >> selector [
	^ selector
]

{ #category : #accessing }
HESendMessage >> selector: anObject [
	selector := anObject
]
