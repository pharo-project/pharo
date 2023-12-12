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
ShSharedVariablesChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e sharedVariables sorted: [ :a :b | a name < b name ] ].
 	classAccessor := [ :e | e classVariables sorted: [ :a :b | a name < b name ] ].
 	comparer := [ :a :b | self compareVariables: a with: b ]
	
]
