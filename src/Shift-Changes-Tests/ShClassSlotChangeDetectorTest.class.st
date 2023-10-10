Class {
	#name : 'ShClassSlotChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-Changes-Tests',
	#package : 'Shift-Changes-Tests'
}

{ #category : 'tests' }
ShClassSlotChangeDetectorTest >> testChangeInSlotsIsDetected [

	newBuilder classSlots: { #classVar1. #classVar2 }.

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected
]

{ #category : 'tests' }
ShClassSlotChangeDetectorTest >> testNoChangeInClassSlotsDetected [

	newBuilder classSlots: { #classVar1 }.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
