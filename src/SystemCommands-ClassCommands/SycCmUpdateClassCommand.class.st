"
Force full update of all browser views
"
Class {
	#name : 'SycCmUpdateClassCommand',
	#superclass : 'SycClassExtraCmCommand',
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'executing' }
SycCmUpdateClassCommand >> execute [

	context tool update.
	self inform: 'Browser updated'
]

{ #category : 'accessing' }
SycCmUpdateClassCommand >> icon [

	^ self iconNamed: #smallUpdate
]

{ #category : 'accessing' }
SycCmUpdateClassCommand >> name [

	^ 'Update'
]
