"
Force full update of all browser views
"
Class {
	#name : 'SycUpdateClassCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'executing' }
SycUpdateClassCommand >> execute [

	context tool update.
	self inform: 'Browser updated'
]

{ #category : 'accessing' }
SycUpdateClassCommand >> iconName [

	^ #smallUpdate
]

{ #category : 'accessing' }
SycUpdateClassCommand >> name [

	^ 'Update'
]
