"
I represent manual command activation to allow reuse activation machinery in cases when user creates command instances manually.
For example I can be used in button kind widgets where model is command activator. 
In that case widgets are able create activator directly from given command using my instance and null tool context.
"
Class {
	#name : #CmdManualActivationStrategy,
	#superclass : #CmdCommandActivationStrategy,
	#category : #'Commander-Core'
}

{ #category : #accessing }
CmdManualActivationStrategy >> menuItemNameFor: aCommand [
	^ aCommand defaultMenuItemName
]
