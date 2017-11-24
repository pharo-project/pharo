"
Action when the method contains a halt
"
Class {
	#name : #MethodContainsHaltAction,
	#superclass : #AbstractMethodIconAction,
	#category : #'Nautilus-Widgets'
}

{ #category : #order }
MethodContainsHaltAction >> actionOrder [
	"Return the priority of this action"
	
	^ 100
]

{ #category : #order }
MethodContainsHaltAction >> actionStateToCache [
	"Return the state of the icon for caching purpose"
	
	^ icon
]

{ #category : #order }
MethodContainsHaltAction >> isActionHandled [

	^ method containsHalt
]

{ #category : #order }
MethodContainsHaltAction >> privateActionIcon [
	"Return the icon for this action"

	^ self iconNamed: #haltIcon
]
