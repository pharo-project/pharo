"
I detect if there is a change in the superclass
"
Class {
	#name : 'ShSuperclassChangeDetector',
	#superclass : 'ShAbstractInstanceSideClassChangeDetector',
	#category : 'Shift-ClassBuilder-Changes',
	#package : 'Shift-ClassBuilder',
	#tag : 'Changes'
}

{ #category : 'initialization' }
ShSuperclassChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e superclass ].
	classAccessor := [ :e | e superclass ]
]
