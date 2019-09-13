"
We store state in a hidden array. Contrary to normal instance variables the number is unlimited.
"
Class {
	#name : #UnlimitedInstanceVariableSlot,
	#superclass : #DerivedSlot,
	#instVars : [
		'offset'
	],
	#category : #'Slot-Examples-Base'
}

{ #category : #'code generation' }
UnlimitedInstanceVariableSlot >> emitStore: methodBuilder [
	"generate bytecode for 'baseSlot at: index put: <stackTop>'"
	| tempName |
	tempName := '0slotTempForStackManipulation'.
	methodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushInstVar: baseSlot index;
		pushLiteral: offset;
		pushTemp: tempName;
		send: #at:put:
]

{ #category : #'code generation' }
UnlimitedInstanceVariableSlot >> emitValue: methodBuilder [
	"generate bytecode for '<baseSlot> at: offset'"
	methodBuilder
		pushInstVar: baseSlot index;
		pushLiteral: offset;
		send: #at:
			
]

{ #category : #'class building' }
UnlimitedInstanceVariableSlot >> growBaseSlot: aClass [
	baseSlot default: (baseSlot default grownBy: 1)
]

{ #category : #'class building' }
UnlimitedInstanceVariableSlot >> installingIn: aClass [
	| unlimitedSlots |
	super installingIn: aClass.
	
	"we reuse a baseSlot if it is already there, if not, we add it"
	"TODO: this does not take into account adding BooleanSlots higher up in the Hierarchy"
	aClass classLayout 
		resolveSlot: #'_ivarArrayBaseSlot'   
		ifFound:  [: slot |  baseSlot := slot. self growBaseSlot: aClass. ]
		ifNone: [aClass addSlot: (baseSlot := #'_ivarArrayBaseSlot' => BaseSlot default: (Array new: 1))].
	
	"my offset in the base slot is defined by the order of all BooleanSlots in the Hierarchy"
	unlimitedSlots := aClass allSlots select: [ :each | each isKindOf: self class ].
	offset := unlimitedSlots indexOf: self

]

{ #category : #accessing }
UnlimitedInstanceVariableSlot >> offset [
	^offset
]

{ #category : #'meta-object-protocol' }
UnlimitedInstanceVariableSlot >> read: anObject [
	^(baseSlot read: anObject) at: offset
		
]

{ #category : #TODO }
UnlimitedInstanceVariableSlot >> todo [
	'
    - adding a slots in between --> offsets need to be updated?
	 - removing slot in the middle	
	 - adding slot higher in the hierarchy: move baseselot (same as in TODO for DictionarySlot)
	 - instance migration?
	'
]

{ #category : #'meta-object-protocol' }
UnlimitedInstanceVariableSlot >> write: aValue to: anObject [
	^(baseSlot read: anObject) at: offset put: aValue.
	
]
