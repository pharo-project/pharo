Class {
	#name : 'StSelectDialogMock',
	#superclass : 'Object',
	#instVars : [
		'selectedItems'
	],
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'accessing' }
StSelectDialogMock >> action [ 
	"I'm a mock"
	^ self
]

{ #category : 'accessing' }
StSelectDialogMock >> selectedItems [

	^ selectedItems 
		ifNil: [ selectedItems := { #iVar1 . #iVar2 } ]
]

{ #category : 'accessing' }
StSelectDialogMock >> selectedItems: anObject [

	selectedItems := anObject
]

{ #category : 'accessing' }
StSelectDialogMock >> title: doesNotCare [
]
