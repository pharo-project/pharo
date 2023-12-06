"
I know how to detect a change in the collection of the shared variables (class variables).
"
Class {
	#name : 'ShSharedVariablesChangeDetector',
	#superclass : 'ShAbstractClassChangeDetector',
	#category : 'Shift-ClassBuilder-Changes',
	#package : 'Shift-ClassBuilder',
	#tag : 'Changes'
}

{ #category : 'initialization' }
ShSharedVariablesChangeDetector >> compareFrom: a to: b [
	^ a asDictionary = b asDictionary
]

{ #category : 'initialization' }
ShSharedVariablesChangeDetector >> initialize [
	super initialize.
	
	builderAccessor := [ :e | e sharedVariables asArray ].
	classAccessor := [ :e | e classVariables asArray ].
	comparer := [ :a :b | self compareVariableCollection: a with: b ]
]
