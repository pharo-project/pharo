Class {
	#name : #ComposedSlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
ComposedSlotTest >> testComposedSlotAutoCompose [
	| slot |
	slot := #slot1 => WeakSlotComposable.
	
	self assert: slot class equals: WeakSlotComposable. 
	"we share the name with the storage slot"
	self assert: slot name equals: #slot1.
	"as our storage slot is a normal ivar, we are not virtual"
	self deny: slot isVirtual.
]

{ #category : #tests }
ComposedSlotTest >> testComposedSlotSimpleCompose [
	| slot |
	slot := #slot1 => InstanceVariableSlot + WeakSlotComposable.
	
	self assert: slot class equals: WeakSlotComposable. 
	"we share the name with the storage slot"
	self assert: slot name equals: #slot1.
	"as our storage slot is a normal ivar, we are not virtual"
	self deny: slot isVirtual.
	
	slot := #slot1 => PropertySlot + WeakSlotComposable.
	
	self assert: slot class equals: WeakSlotComposable. 
	"we share the name with the storage slot"
	self assert: slot name equals: #slot1.
	"as our storage slot is a Property, we are virtual"
	self assert: slot isVirtual.
]
