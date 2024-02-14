"
I am an exception to indicate that a slot is found in a layout or in the image.
"
Class {
	#name : 'SlotNotFound',
	#superclass : 'Error',
	#instVars : [
		'name'
	],
	#category : 'Kernel-CodeModel-Exceptions',
	#package : 'Kernel-CodeModel',
	#tag : 'Exceptions'
}

{ #category : 'signalling' }
SlotNotFound class >> signalForName: aSymbol [
	self new
		name: aSymbol;
		signal
]

{ #category : 'accessing' }
SlotNotFound >> messageText [
	^ 'Could not build slot named ''' , name asString , ''' because the assignated slot was not found in the image.'
]

{ #category : 'accessing' }
SlotNotFound >> name [
	^ name
]

{ #category : 'accessing' }
SlotNotFound >> name: anObject [
	name := anObject
]
