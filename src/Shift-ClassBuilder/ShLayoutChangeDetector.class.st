"
I know how to detect a change in the layout of a class.
"
Class {
	#name : 'ShLayoutChangeDetector',
	#superclass : 'ShAbstractInstanceSideClassChangeDetector',
	#category : 'Shift-ClassBuilder-Changes',
	#package : 'Shift-ClassBuilder',
	#tag : 'Changes'
}

{ #category : 'initialization' }
ShLayoutChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e layoutClass ].
	classAccessor := [ :e | e classLayout class ]
]
