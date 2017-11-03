"
Action when the method is abstract
"
Class {
	#name : #MethodIsAbstractAction,
	#superclass : #AbstractMethodIconAction,
	#category : #Nautilus-Widgets
}

{ #category : #order }
MethodIsAbstractAction >> actionOrder [
	"Return the priority of this action"
	
	^ 700
]

{ #category : #order }
MethodIsAbstractAction >> actionStateToCache [
	"Return the state of the icon for caching purpose"
	
	^ icon
]

{ #category : #order }
MethodIsAbstractAction >> isActionHandled [

	^ method isAbstract
]

{ #category : #order }
MethodIsAbstractAction >> privateActionIcon [
	"Return the icon for this action"
	
	^ self iconNamed: #abstractIcon
]
