"
I know how to detect a change in the layout of a class.
"
Class {
	#name : #ShLayoutChangeDetector,
	#superclass : #ShAbstractInstanceSideClassChangeDetector,
	#category : 'Shift-Changes'
}

{ #category : #initialization }
ShLayoutChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e layoutDefinition layoutClass ].
	classAccessor := [ :e | e classLayout class ]
]
