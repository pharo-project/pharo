"
I know how to detect a change in the collection of slots.
"
Class {
	#name : 'ShSlotChangeDetector',
	#superclass : 'ShAbstractInstanceSideClassChangeDetector',
	#category : 'Shift-ClassBuilder-Changes',
	#package : 'Shift-ClassBuilder',
	#tag : 'Changes'
}

{ #category : 'initialization' }
ShSlotChangeDetector >> initialize [

	super initialize.
	builderAccessor := [ :e | e allSlots asArray ].
	classAccessor := [ :e | e allSlots asArray ].
	comparer := [ :a :b | self compareVariableCollection: a with: b ]
]
