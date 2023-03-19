"
I get signalled when a temporary variable is used that is not defined.  My default action is to create an Undeclared binding and add it to the Undeclared dictionary.
"
Class {
	#name : #OCUndeclaredVariableWarning,
	#superclass : #OCSemanticWarning,
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

	^ super defaultAction
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> reparator [

	^ OCCodeReparator new node: node
]
