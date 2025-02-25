"
Command to unselect all items in the `StClassAndMethodsSelectionPresenter`. It also takes care of updating the selection status in the UI.
"
Class {
	#name : 'StClassAndMethodsSelectNoneCommand',
	#superclass : 'StClassAndMethodsCommand',
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'default' }
StClassAndMethodsSelectNoneCommand class >> defaultDescription [

	^ 'Unselect all methods'
]

{ #category : 'default' }
StClassAndMethodsSelectNoneCommand class >> defaultIconName [

	^ #checkboxUnselected
]

{ #category : 'default' }
StClassAndMethodsSelectNoneCommand class >> defaultName [

	^ 'Select None'
]

{ #category : 'executing' }
StClassAndMethodsSelectNoneCommand >> execute [
	"Unselect all marked methods"

	self isInstanceMethodsSelected
		ifTrue: [ 
			self context selectedInstanceMethods: Set empty.
			self instanceMethodsTable owner update. ]
		ifFalse: [ 
			self context selectedClassMethods: Set empty.
			self classMethodsTable owner update. ].

]
