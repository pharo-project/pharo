"
Action when the method contains a flag
"
Class {
	#name : #MethodContainsFlagsAction,
	#superclass : #AbstractMethodIconAction,
	#category : 'Nautilus-Widgets'
}

{ #category : #order }
MethodContainsFlagsAction >> actionOrder [
	"Return the priority of this action"

	^ 300
]

{ #category : #order }
MethodContainsFlagsAction >> actionStateToCache [
	"Return the state of the icon for caching purpose"
	
	^ icon
]

{ #category : #order }
MethodContainsFlagsAction >> isActionHandled [ 

	^ method sendsAnySelectorOf: #(flag:)
]

{ #category : #order }
MethodContainsFlagsAction >> privateActionIcon [
	"Return the icon for this action"

	^ self iconNamed: #flagIcon
]
