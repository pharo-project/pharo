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
	"old code, to be cleaned when the new way works"
	"builderAccessor := [ :e | e sharedVariables collect: [ :x | x key -> x class ] ].
	classAccessor := [ :e | e classVariables collect: [ :x | x key -> x class ] ].
	comparer := [ :a :b | self compareFrom: a to: b ]"
	
	"We should use #compareVariableCollection:with: to compare. To check why the bootstrap fails"
	
	builderAccessor := [ :e | e sharedVariables ].
 	classAccessor := [ :e | e classVariables ].
 	comparer := [ :a :b | self compareVariables: a with: b ]
	
]
