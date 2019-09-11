"
I am a Slot that does not allocate one field for each slot. Instead, all PropertySlots of the whole hierarchy are allocated in a dictionary that is stored in an invisible slot (see PropertyBaseSlot)

Keep in mind:
- I am slower than instance variables
- there is the overhead of the Dictionary. Only if there are multiple ones of me in the class hierarchy you will save memory
"
Class {
	#name : #PropertySlot,
	#superclass : #DerivedSlot,
	#category : #'Slot-Examples-Base'
}

{ #category : #'code generation' }
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

{ #category : #'code generation' }
PropertySlot >> emitValue: methodBuilder [
	"generate bytecode for '<baseSlot> at: name ifAbsent: nil'"
	methodBuilder
		pushInstVar: baseSlot index;
		pushLiteral: self name;
		pushLiteral: nil;
		send: #at:ifAbsent:
	
	
]

{ #category : #'class building' }
PropertySlot >> installingIn: aClass [
	super installingIn: aClass.
	
	"we reuse a baselot if it is already there, if not, we add it"
	"TODO: this does not take into account adding PropertySlot higher up in the Hierarchy"
	aClass classLayout 
		resolveSlot: #'_propertyBaseSlot'   
		ifFound:  [: slot |  baseSlot := slot ]
		ifNone: [aClass addSlot: (baseSlot := #'_propertyBaseSlot' => BaseSlot default: Dictionary new)]
	
]

{ #category : #'meta-object-protocol' }
PropertySlot >> read: anObject [
	^ (baseSlot read: anObject) at: self name ifAbsent: nil
]

{ #category : #'meta-object-protocol' }
PropertySlot >> write: aValue to: anObject [
	^ (baseSlot read: anObject) at: self name put: aValue
]
