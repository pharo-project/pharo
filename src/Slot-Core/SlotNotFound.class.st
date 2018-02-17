"
I am an exception to indicate that a slot is found in a layout.
"
Class {
	#name : #SlotNotFound,
	#superclass : #ClassBuilderError,
	#instVars : [
		'name',
		'targetClass'
	],
	#category : #'Slot-Core-Exception'
}

{ #category : #signalling }
SlotNotFound class >> signalForName: aSymbol [
	self new
		name: aSymbol;
		signal
]

{ #category : #signalling }
SlotNotFound class >> signalForName: aSymbol inClass: aClass [
	self new
		name: aSymbol;
		targetClass: aClass;
		signal
]

{ #category : #accessing }
SlotNotFound >> messageText [
	^ 'Slot ''', name asString, ''' not found', (targetClass ifNil: [''] ifNotNil:[' in ', targetClass name])
]

{ #category : #accessing }
SlotNotFound >> name [
	^ name
]

{ #category : #accessing }
SlotNotFound >> name: anObject [
	name := anObject
]

{ #category : #accessing }
SlotNotFound >> targetClass [
	^ targetClass
]

{ #category : #accessing }
SlotNotFound >> targetClass: aClass [
	targetClass := aClass
]
