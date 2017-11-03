"
Notify about an unused temporary variable
"
Class {
	#name : #UnusedVariableDetected,
	#superclass : #ParserNotification,
	#category : #Compiler-Exceptions
}

{ #category : #correcting }
UnusedVariableDetected >> openMenuIn: aBlock [
	| index |
	index := aBlock value: #('yes' 'no')
					value: #()
					value: name, ' appears to be\unused in this method.\OK to remove it?' withCRs.
	self resume: index = 1
]
