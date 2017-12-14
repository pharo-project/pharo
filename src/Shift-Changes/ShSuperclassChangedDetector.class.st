"
I detect if there is a change in the superclass
"
Class {
	#name : #ShSuperclassChangedDetector,
	#superclass : #ShAbstractClassChangeDetector,
	#category : 'Shift-Changes'
}

{ #category : #initialization }
ShSuperclassChangedDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e superclass ].
	classAccessor := [ :e | e superclass ]
]
