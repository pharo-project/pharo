Class {
	#name : 'StSelectDialogMock',
	#superclass : 'Object',
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

	^ { #iVar1 . #iVar2 }
]

{ #category : 'accessing' }
StSelectDialogMock >> title: doesNotCare [
]
