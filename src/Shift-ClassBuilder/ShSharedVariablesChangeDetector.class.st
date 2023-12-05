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
	
	builderAccessor := [ :e | e layoutDefinition sharedVariables collect:[ :x | x key -> x class ] as: Array ].
	classAccessor := [ :e | e classVariables collect: [ :x | x key -> x class ] as: Array ].
	comparer := [ :a :b | self compareFrom: a to: b ]
	
	"TODO
	This should use #compareVariableCollection:with:, but then we detect changes that lead to a crash during full image reload, possibly to the code in #migrateClassTo: that uses become,
	
	builderAccessor := [ :e | e sharedVariables asArray ].
	classAccessor := [ :e | e classVariables asArray ].
	comparer := [ :a :b | self compareVariableCollection: a with: b ]
	"
]
