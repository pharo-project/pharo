"
I am a Slot that does not allocate one field for each slot. Instead, all PropertySlots of the whole hierarchy are allocated in a dictionary that is stored in an invisible slot (see BaseSlot)

Keep in mind:
- I am slower than instance variables
- there is the overhead of the Dictionary. Only if there are multiple ones of me in the class hierarchy you will save memory
"
Class {
	#name : 'PropertySlot',
	#superclass : 'DerivedSlot',
	#category : 'Slot-Examples-Base',
	#package : 'Slot-Examples',
	#tag : 'Base'
}

{ #category : 'private' }
PropertySlot >> addAdditionalSlotsTo: slots [
	"we reuse a baseSlot if it is already there, if not, we add it"

	"TODO: this does not take into account adding BooleanSlots higher up in the Hierarchy"

	slots
		detect: [ :slot | slot name = #_propertyBaseSlot ]
		ifFound: [ :slot | baseSlot := slot ]
		ifNone: [ slots add: (baseSlot := #_propertyBaseSlot => BaseSlot default: Dictionary new) ]
]

{ #category : 'code generation' }
PropertySlot >> emitStore: methodBuilder [
	"generate bytecode for 'baseSlot at: 1 put: <stackTop>'"
	| tempName |
	tempName := '0slotTempForStackManipulation'.
	methodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushInstVar: baseSlot index;
		pushLiteral: self name;
		pushTemp: tempName;
		send: #at:put:
]

{ #category : 'code generation' }
PropertySlot >> emitValue: methodBuilder [
	"generate bytecode for '<baseSlot> at: name ifAbsent: nil'"
	methodBuilder
		pushInstVar: baseSlot index;
		pushLiteral: self name;
		pushLiteral: nil;
		send: #at:ifAbsent:
]

{ #category : 'meta-object-protocol' }
PropertySlot >> read: anObject [
	^ (baseSlot read: anObject) at: self name ifAbsent: nil
]

{ #category : 'meta-object-protocol' }
PropertySlot >> write: aValue to: anObject [
	^ (baseSlot read: anObject) at: self name put: aValue
]
