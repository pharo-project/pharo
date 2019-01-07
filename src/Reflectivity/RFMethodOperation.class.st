"
I am representing a method ready to be executed. I know the receiver and the arguments, #value will execute the method
"
Class {
	#name : #RFMethodOperation,
	#superclass : #Object,
	#instVars : [
		'method',
		'arguments',
		'receiver'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #accessing }
RFMethodOperation >> arguments [
	^ arguments
]

{ #category : #accessing }
RFMethodOperation >> arguments: anObject [
	arguments := anObject
]

{ #category : #accessing }
RFMethodOperation >> method [
	^ method
]

{ #category : #accessing }
RFMethodOperation >> method: anObject [
	method := anObject
]

{ #category : #accessing }
RFMethodOperation >> receiver [
	^ receiver
]

{ #category : #accessing }
RFMethodOperation >> receiver: anObject [
	receiver := anObject
]

{ #category : #evaluating }
RFMethodOperation >> value [
	^method valueWithReceiver: receiver arguments: arguments  
]
