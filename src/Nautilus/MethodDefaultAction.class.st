"
Action by default, empty icon
"
Class {
	#name : #MethodDefaultAction,
	#superclass : #AbstractMethodIconAction,
	#category : 'Nautilus-Widgets'
}

{ #category : #order }
MethodDefaultAction >> actionOrder [
	"Return the priority of this action"
	
	^ 9999999999999999999999999999
]

{ #category : #order }
MethodDefaultAction >> actionStateToCache [
	"Return the state of the icon for caching purpose"
	
	^ icon
]

{ #category : #order }
MethodDefaultAction >> isActionHandled [
	"Return true if the provided method fits this action requirement"

	^ true
]

{ #category : #order }
MethodDefaultAction >> privateActionIcon [
	"Return the icon for this action"

	^ self iconNamed: #emptyIcon
]
