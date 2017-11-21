"
I am a Slot that does not allocate one field for each slot. Instead, all PropertySlots of the whole hierarchy are allocated in a dictionary that is stored in an invisible slot (see PropertyBaseSlot)

Keep in mind:
- I am slower than instance variables
- there is the overhead of the Dictionary. Only if there are multiple ones of me in the class hierarchy you will save memory
"
Class {
	#name : #PropertySlot,
	#superclass : #Slot,
	#instVars : [
		'baseSlot'
	],
	#category : #Slot-Examples
}

{ #category : #'code generation' }
PropertySlot >> emitStore: methodBuilder [
	| tempName |

	tempName := Object new.
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

		methodBuilder
			pushInstVar: baseSlot index;
			pushLiteral: self name;
			pushLiteral: nil;
			send: #at:ifAbsent:
	
	
]

{ #category : #'class building' }
PropertySlot >> layoutChanged: aClass [
	super layoutChanged: aClass.
	"we check on the slot level as the slot is hidden"
	(baseSlot notNil and: [aClass classLayout hasSlotNamed: baseSlot name]) ifTrue: [
		baseSlot := aClass classLayout slotNamed: baseSlot name. "make sure we all use the same baseSlot"
		^self. ].
	"here we need to be more clever: if a subclass defines it, 
	move the difiniton here"
	self flag: #TBD.
	aClass addSlot: (baseSlot := #'_propertyBaseSlot' => BaseSlot default: Dictionary new).
	
]

{ #category : #meta-object-protocol }
PropertySlot >> read: anObject [
	^ (baseSlot read: anObject) at: self name ifAbsent: nil
]

{ #category : #meta-object-protocol }
PropertySlot >> write: aValue to: anObject [
	^ (baseSlot read: anObject) at: self name put: aValue
]
