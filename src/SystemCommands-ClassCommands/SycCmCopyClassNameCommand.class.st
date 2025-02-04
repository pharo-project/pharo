"
Command that implements the copy the class name to the clipboard
"
Class {
	#name : 'SycCmCopyClassNameCommand',
	#superclass : 'SycClassExtraCmCommand',
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'testing' }
SycCmCopyClassNameCommand >> canBeExecuted [ 

	^ self selectedClass notNil
]

{ #category : 'executing' }
SycCmCopyClassNameCommand >> execute [
	| name |
	
	name := self selectedClass name.
	Clipboard clipboardText: name.
	self inform: ('The class name {1} is now in the clipboard' format: (Array with: name))
]

{ #category : 'accessing' }
SycCmCopyClassNameCommand >> icon [
	^ self iconNamed: #smallCopy
]

{ #category : 'accessing' }
SycCmCopyClassNameCommand >> name [

	^ 'Copy class name to Clipboard'
]

{ #category : 'accessing' }
SycCmCopyClassNameCommand >> order [
	^ 46
]

{ #category : 'testing' }
SycCmCopyClassNameCommand >> selectedClass [
	"Answer the last selected <Class>"

	^ context lastSelectedClass realClass
]
