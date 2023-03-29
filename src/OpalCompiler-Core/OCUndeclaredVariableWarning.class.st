"
I get signalled when a temporary variable is used that is not defined.  My default action is to create an Undeclared binding and add it to the Undeclared dictionary.
"
Class {
	#name : #OCUndeclaredVariableWarning,
	#superclass : #Notification,
	#instVars : [
		'notice'
	],
	#category : #'OpalCompiler-Core-Exception'
}

{ #category : #handling }
OCUndeclaredVariableWarning >> defaultAction [
	| className selector |
 	className := self methodClass name.
	selector := self methodNode selector.

	NewUndeclaredWarning signal: self node name in: (selector
		ifNotNil: [className, '>>', selector]
			ifNil: ['<unknown>']).

	^ super defaultAction
]

{ #category : #accessing }
OCUndeclaredVariableWarning >> methodClass [
	^ self methodNode methodClass
]

{ #category : #accessing }
OCUndeclaredVariableWarning >> methodNode [
	^ self node methodNode
]

{ #category : #accessing }
OCUndeclaredVariableWarning >> node [
	^ self notice node
]

{ #category : #accessing }
OCUndeclaredVariableWarning >> notice [

	^ notice
]

{ #category : #accessing }
OCUndeclaredVariableWarning >> notice: anObject [

	notice := anObject
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> reparator [

	^ OCCodeReparator new node: self node
]
