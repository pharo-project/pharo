Class {
	#name : 'ShSlotChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-ClassBuilder-Tests-Changes',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Changes'
}

{ #category : 'tests' }
ShSlotChangeDetectorTest >> testChangeInSlotsIsDetected [

	newBuilder slots: { #ivar1. #ivar2 }.

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected
]

{ #category : 'tests' }
ShSlotChangeDetectorTest >> testNoChangeInSlotsDetected [

	newBuilder slots: { #ivar1 }.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
