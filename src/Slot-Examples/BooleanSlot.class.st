"
**Unfinished, see #todo**

I am a Slot that does not allocate one field for each slot. Instead, all Booleanlots of the whole hierarchy are allocated in an integrer that is stored in an invisible slot .


"
Class {
	#name : #BooleanSlot,
	#superclass : #Slot,
	#instVars : [
		'baseSlot',
		'offset'
	],
	#category : #'Slot-Examples-Base'
}

{ #category : #accessing }
BooleanSlot >> baseSlotRead: anObject [
	^ (baseSlot read: anObject) ifNil: [ 0 ].
	
]

{ #category : #'code generation' }
BooleanSlot >> emitStore: methodBuilder [
	| tempName |

	tempName := Object new.
	methodBuilder
		send: #asBit;
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushInstVar: baseSlot index;
		pushLiteral: offset;
		pushTemp: tempName;
		send: #bitAt:put:;
		storeInstVar: baseSlot index
	
]

{ #category : #'code generation' }
BooleanSlot >> emitValue: methodBuilder [

		methodBuilder
			pushInstVar: baseSlot index;
			pushLiteral: offset;
			send: #bitAt:;
			pushLiteral: 1;
			send: #==.
	
	
]

{ #category : #'class building' }
BooleanSlot >> installingIn: aClass [
	| booleanSlots |
	super installingIn: aClass.
	
	"we reuse a baselot if it is already there, if not, we add it"
	"TODO: this does not take into account adding BooleanSlots higher up in the Hierarchy"
	aClass classLayout 
		resolveSlot: #'_booleanBaseSlot'   
		ifFound:  [: slot |  baseSlot := slot ]
		ifNone: [aClass addSlot: (baseSlot := #'_booleanBaseSlot' => BaseSlot default: 0)].
	
	"my offset in the base slot is defined by the order of all BooleanSlots in the Hierarchy"
	booleanSlots := aClass allSlots select: [ :each | each isKindOf: self class ].	
	offset := booleanSlots indexOf: self.

	

	
]

{ #category : #accessing }
BooleanSlot >> offset [
	^offset
]

{ #category : #'meta-object-protocol' }
BooleanSlot >> read: anObject [
	^((self baseSlotRead: anObject) bitAt: offset) == 1
]

{ #category : #TODO }
BooleanSlot >> todo [
	'
    - adding a Booleanslot in between --> offsets need to be updated?
	 - removing BooleanSlot in the middle	
	 - adding booleanslot higher in the hierarchy: move baseselot (same as in TODO for DictionarySlot)
	'
]

{ #category : #'meta-object-protocol' }
BooleanSlot >> write: aValue to: anObject [
		| newInteger |
		newInteger := (self baseSlotRead: anObject) bitAt: offset put: aValue asBit.
		baseSlot write: newInteger to: anObject.
		^aValue
]
