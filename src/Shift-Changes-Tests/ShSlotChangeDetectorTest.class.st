Class {
	#name : 'ShSlotChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-Changes-Tests',
	#package : 'Shift-Changes-Tests'
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
