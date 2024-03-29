"
I am signaled when trying to build a class with a duplicated slot.
"
Class {
	#name : 'DuplicatedSlotName',
	#superclass : 'Error',
	#instVars : [
		'newSlot',
		'oldSlot',
		'host'
	],
	#category : 'Kernel-CodeModel-Exceptions',
	#package : 'Kernel-CodeModel',
	#tag : 'Exceptions'
}

{ #category : 'accessing' }
DuplicatedSlotName >> host [
	^ host
]

{ #category : 'accessing' }
DuplicatedSlotName >> host: anObject [
	host := anObject
]

{ #category : 'accessing' }
DuplicatedSlotName >> messageText [
	^ 'Slot #''', oldSlot name, ''' appeared twice in ', host name
]

{ #category : 'accessing' }
DuplicatedSlotName >> newSlot [
	^ newSlot
]

{ #category : 'accessing' }
DuplicatedSlotName >> newSlot: anObject [
	newSlot := anObject
]

{ #category : 'accessing' }
DuplicatedSlotName >> oldSlot [
	^ oldSlot
]

{ #category : 'accessing' }
DuplicatedSlotName >> oldSlot: anObject [
	oldSlot := anObject
]
