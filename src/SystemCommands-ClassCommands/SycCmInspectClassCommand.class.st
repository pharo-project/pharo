"
Inspect current selected class in the browser.
If there are multiple selection, the command will open an inspector on the selected list of classes. If there there is a single selection, it will open the inspector directly on the selected class.

Internal Representation and Key Implementation Points.

## Instance Variables

	- selectedItems:		Collection of <Class>
"
Class {
	#name : 'SycCmInspectClassCommand',
	#superclass : 'SycClassExtraCmCommand',
	#instVars : [
		'selectedItems'
	],
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'executing' }
SycCmInspectClassCommand >> executeRefactoring [ 

	selectedItems size = 1
		ifTrue: [ selectedItems first inspect ]
		ifFalse: [ selectedItems inspect]
]

{ #category : 'accessing' }
SycCmInspectClassCommand >> icon [

	^ self iconNamed: #smallInspectIt
]

{ #category : 'accessing' }
SycCmInspectClassCommand >> name [

	^ 'Inspect'
]

{ #category : 'preparation' }
SycCmInspectClassCommand >> prepareFullExecution [

	super prepareFullExecution.
	selectedItems := context selectedObjects
]
