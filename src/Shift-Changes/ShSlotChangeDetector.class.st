"
I know how to detect a change in the collection of slots.
"
Class {
	#name : #ShSlotChangeDetector,
	#superclass : #ShAbstractInstanceSideClassChangeDetector,
	#category : 'Shift-Changes'
}

{ #category : #comparing }
ShSlotChangeDetector >> compareSlotCollection: a with: b [
	(a size = b size) ifFalse:[^false].

	a with: b do: [ :aSlot :bSlot | 
			(aSlot name = bSlot name and: [ aSlot species = bSlot species ])
				ifFalse: [ ^ false ] ].

	^ true
]

{ #category : #initialization }
ShSlotChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e layoutDefinition slots asArray ].
	classAccessor := [ :e | e slots asArray].
	comparer := [ :a :b |  self compareSlotCollection: a with: b ]
]
