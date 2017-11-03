"
Common superclass of Gofer operations
"
Class {
	#name : #GoferOperation,
	#superclass : #Object,
	#instVars : [
		'gofer',
		'model'
	],
	#category : #Gofer-Core
}

{ #category : #'instance creation' }
GoferOperation class >> new [
	self error: 'Gofer operations can only work on Gofer instances.'
]

{ #category : #'instance creation' }
GoferOperation class >> on: aGofer [
	^ self basicNew initializeOn: aGofer
]

{ #category : #private }
GoferOperation >> defaultModel [
	^ nil
]

{ #category : #running }
GoferOperation >> execute [
	"Execute the receiving action."

	self subclassResponsibility
]

{ #category : #accessing }
GoferOperation >> gofer [
	"Answer the Gofer instance that triggered this operation."

	^ gofer
]

{ #category : #initialization }
GoferOperation >> initialize [
	model := self defaultModel
]

{ #category : #initialization }
GoferOperation >> initializeOn: aGofer [
	gofer := aGofer.
	self initialize
]

{ #category : #accessing }
GoferOperation >> model [
	"Answer the Monticello model of this operation."
	
	^ model
]
