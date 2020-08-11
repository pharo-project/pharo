"
! StShowLineNumbersCommand
Toggles the show line number state (to hide or show line numbers column).

[[[language=Smalltalk
(StShowLineNumbersCommand forSpecContext: (StPlayground new)) execute
]]]
"
Class {
	#name : #StShowLineNumbersCommand,
	#superclass : #StCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StShowLineNumbersCommand class >> defaultDescription [

	^ 'Show/hide line numbers'
]

{ #category : #default }
StShowLineNumbersCommand class >> defaultName [

	^ 'Show line numbers'
]

{ #category : #private }
StShowLineNumbersCommand class >> hideLineNumbersLabel [

	^ 'Hide line numbers'
]

{ #category : #default }
StShowLineNumbersCommand class >> iconLabelFor: aBoolean [
	
	^ aBoolean 
		ifTrue: [ '+L' ]
		ifFalse: [ 'L' ]
]

{ #category : #private }
StShowLineNumbersCommand class >> showLineNumbersLabel [

	^ 'Show line numbers'
]

{ #category : #executing }
StShowLineNumbersCommand >> execute [

	context toggleShowLineNumbers
]

{ #category : #accessing }
StShowLineNumbersCommand >> name [

	^ context showLineNumbers
		ifTrue: [ self class hideLineNumbersLabel ]
		ifFalse: [ self class showLineNumbersLabel ]
]
