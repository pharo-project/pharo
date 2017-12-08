"
I know how to detect a change in the collection of slots.
"
Class {
	#name : #ShSlotChangeDetector,
	#superclass : #ShAbstractInstanceSideClassChangeDetector,
	#category : 'Shift-Changes'
}

{ #category : #initialization }
ShSlotChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e layoutDefinition slots asArray ].
	classAccessor := [ :e | e slots asArray].
	comparer := [ :a :b |  self compareSlotCollection: a with: b ]
]
