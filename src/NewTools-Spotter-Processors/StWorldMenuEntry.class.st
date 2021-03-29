"
I wrap a World menu entry, to modify its behavior if required for the spotter
"
Class {
	#name : #StWorldMenuEntry,
	#superclass : #StEntry,
	#category : #'NewTools-Spotter-Processors-Entries'
}

{ #category : #converting }
StWorldMenuEntry >> asString [ 

	^ content contents
]

{ #category : #evaluating }
StWorldMenuEntry >> doEvaluate [ 

	content spotterActDefault
]

{ #category : #accessing }
StWorldMenuEntry >> icon [

	^ content icon
]
