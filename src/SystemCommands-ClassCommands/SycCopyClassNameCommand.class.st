"
Command that implements the copy the class name to the clipboard
"
Class {
	#name : 'SycCopyClassNameCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'testing' }
SycCopyClassNameCommand >> canBeExecuted [ 

	^ self selectedClass notNil
]

{ #category : 'executing' }
SycCopyClassNameCommand >> execute [
	| name |
	
	name := self selectedClass name.
	Clipboard clipboardText: name.
	self inform: ('The class name {1} is now in the clipboard' format: (Array with: name))
]

{ #category : 'accessing' }
SycCopyClassNameCommand >> iconName [

	^ #smallCopy
]

{ #category : 'accessing' }
SycCopyClassNameCommand >> name [

	^ 'Copy class name to Clipboard'
]

{ #category : 'accessing' }
SycCopyClassNameCommand >> order [
	^ 46
]

{ #category : 'testing' }
SycCopyClassNameCommand >> selectedClass [
	"Answer the last selected <Class>"

	^ context lastSelectedClass realClass
]
