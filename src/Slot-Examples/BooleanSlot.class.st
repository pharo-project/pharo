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
	#category : #'Slot-Examples-base'
}

{ #category : #accessing }
BooleanSlot >> baseSlotRead: anObject [
	^ (baseSlot read: anObject) ifNil: [ 0 ].
	
]

{ #category : #'class building' }
BooleanSlot >> calculateOffset: aClass [
	| booleanSlots  calculatedOffset |
	"first we get all boolean slots and sort them by index"
	booleanSlots := aClass allSlots select: [ :each | each isKindOf: self class ].
	booleanSlots := booleanSlots select: [ :each | each offset isNotNil ].
	booleanSlots sort: [ :a :b | a offset < b offset ]. 
	booleanSlots ifEmpty: [ ^1 ]. "we are the first one!"

	"now we find the first spot where the next offset is not yet taken"
	calculatedOffset := (booleanSlots withIndexCollect: [ :each :myIndex | myIndex ->each offset ]) 
		findFirst: [ :each | each key ~= each value ].
	calculatedOffset = 0  "no hole found: use the next index after the last slot"
		ifTrue: [ ^ booleanSlots last offset + 1 ].
	^calculatedOffset
	
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
	super installingIn: aClass.
	offset := self calculateOffset: aClass.
]

{ #category : #'class building' }
BooleanSlot >> layoutChanged: aClass [
	super layoutChanged: aClass.
	"we check on the slot level as the slot is hidden"
	(baseSlot notNil and: [aClass classLayout hasSlotNamed: baseSlot name]) ifTrue: [
		baseSlot := aClass classLayout slotNamed: baseSlot name. "make sure we all use the same baseSlot"
		^self. ].
	"here we need to be more clever: if a subclass defines it, 
	move the difiniton here"
	self flag: #TBD.
	aClass addSlot: (baseSlot := #'_booleanBaseSlot' => BaseSlot default: 0).
	
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
