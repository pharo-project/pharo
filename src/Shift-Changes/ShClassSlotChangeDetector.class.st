"
I know how to detect a change in the collection of slots of the metaclass.
"
Class {
	#name : #ShClassSlotChangeDetector,
	#superclass : #ShAbstractClassChangeDetector,
	#category : 'Shift-Changes'
}

{ #category : #changes }
ShClassSlotChangeDetector >> compareSlotCollection: a with: b [
	(a size = b size) ifFalse:[^false].

	a with: b do: [ :aSlot :bSlot | 
			(aSlot name = bSlot name and: [ aSlot species = bSlot species ])
				ifFalse: [ ^ false ] ].

	^ true
]

{ #category : #initialization }
ShClassSlotChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e layoutDefinition classSlots ].
	classAccessor := [ :e | e class slots ].
	comparer := [ :a :b |  self compareSlotCollection: a with: b ]	
]

{ #category : #changes }
ShClassSlotChangeDetector >> newChanges [
	^ {(ShMetaclassChanged new
		builder: builder;
		yourself)}
]
