"
I detect if there is a change in the superclass
"
Class {
	#name : #ShSuperclassChangedDetector,
	#superclass : #ShAbstractInstanceSideClassChangeDetector,
	#category : #'Shift-Changes'
}

{ #category : #initialization }
ShSuperclassChangedDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e superclass ].
	classAccessor := [ :e | e superclass ]
]
