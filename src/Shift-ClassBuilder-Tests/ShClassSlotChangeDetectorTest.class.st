Class {
	#name : 'ShClassSlotChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-ClassBuilder-Tests-Changes',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Changes'
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
