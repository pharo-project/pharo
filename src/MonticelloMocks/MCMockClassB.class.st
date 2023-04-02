"
This comment has a bang! Bang! Bang!
"
Class {
	#name : #MCMockClassB,
	#superclass : #MCMock,
	#instVars : [
		'ivarb'
	],
	#classVars : [
		'CVar'
	],
	#pools : [
		'MCMockAPoolDictionary'
	],
	#classInstVars : [
		'ciVar'
	],
	#category : #MonticelloMocks
}

{ #category : #accessing }
MCMockClassB class >> cVar [

	^ CVar
]

{ #category : #accessing }
MCMockClassB class >> ciVar [

	^ ciVar
]

{ #category : #accessing }
MCMockClassB >> ivarb [

	^ ivarb
]
