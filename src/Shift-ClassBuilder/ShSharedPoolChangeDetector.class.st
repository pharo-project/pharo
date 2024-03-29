"
I know how to detect a change in the shared pools.
"
Class {
	#name : 'ShSharedPoolChangeDetector',
	#superclass : 'ShAbstractClassChangeDetector',
	#category : 'Shift-ClassBuilder-Changes',
	#package : 'Shift-ClassBuilder',
	#tag : 'Changes'
}

{ #category : 'initialization' }
ShSharedPoolChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e layoutDefinition sharedPools asArray ].
	classAccessor := [ :e | (e sharedPools collect:[:x | x name]) asArray ]
]
