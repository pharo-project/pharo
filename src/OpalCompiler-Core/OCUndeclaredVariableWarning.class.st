"
I get signalled when a temporary variable is used that is not defined.  My default action is to create an Undeclared binding and add it to the Undeclared dictionary.
"
Class {
	#name : #OCUndeclaredVariableWarning,
	#superclass : #OCSemanticWarning,
	#instVars : [
		'variable'
	],
	#category : #'OpalCompiler-Core-Exception'
}

{ #category : #handling }
OCUndeclaredVariableWarning >> defaultAction [
	| className selector |
 	className := self methodClass name.
	selector := self methodNode selector.

	NewUndeclaredWarning signal: node name in: (selector
		ifNotNil: [className, '>>', selector]
			ifNil: ['<unknown>']).

	^ variable
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> reparator [

	^ OCCodeReparator new node: node
]

{ #category : #accessing }
OCUndeclaredVariableWarning >> variable [

	^ variable
]

{ #category : #accessing }
OCUndeclaredVariableWarning >> variable: anObject [

	variable := anObject
]
