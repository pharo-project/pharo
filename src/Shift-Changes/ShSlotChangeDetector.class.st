"
I know how to detect a change in the collection of slots.
"
Class {
	#name : 'ShSlotChangeDetector',
	#superclass : 'ShAbstractInstanceSideClassChangeDetector',
	#category : 'Shift-Changes',
	#package : 'Shift-Changes'
}

{ #category : 'initialization' }
ShSlotChangeDetector >> initialize [

	super initialize.
	builderAccessor := [ :e | e allSlots asArray ].
	classAccessor := [ :e | e allSlots asArray ].
	comparer := [ :a :b | self compareSlotCollection: a with: b ]
]
