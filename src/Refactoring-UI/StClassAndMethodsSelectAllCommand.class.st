"
Command to select all items in the `StClassAndMethodsSelectionPresenter`. It also takes care of updating the selection in the UI.
"
Class {
	#name : 'StClassAndMethodsSelectAllCommand',
	#superclass : 'StClassAndMethodsCommand',
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'default' }
StClassAndMethodsSelectAllCommand class >> defaultDescription [

	^ 'Select all methods'
]

{ #category : 'initialization' }
StClassAndMethodsSelectAllCommand class >> defaultIconName [

	^ #checkedBox
]

{ #category : 'default' }
StClassAndMethodsSelectAllCommand class >> defaultName [

	^ 'Select All'
]

{ #category : 'executing' }
StClassAndMethodsSelectAllCommand >> execute [
	"Select all methods"

	self isInstanceMethodsSelected
		ifTrue: [ 
			self context selectedInstanceMethods: self instanceMethodsTableItems.
			self instanceMethodsTable owner update. ]
		ifFalse: [ 
			self context selectedClassMethods: self classMethodsTableItems.
			self classMethodsTable owner update. ].
	
]
