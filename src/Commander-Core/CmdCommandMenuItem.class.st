"
I represent command in menu.
I am created with command activator and delegate all messages to it to support menu protocol.
 
To create my instances use:
	CmdCommandMenuItem activatingBy: aCommandActivator
	
Internal Representation and Key Implementation Points.

    Instance Variables
	activator:		<CmdCommandActivator>
"
Class {
	#name : #CmdCommandMenuItem,
	#superclass : #CmdMenuItem,
	#instVars : [
		'activator'
	],
	#category : #'Commander-Core'
}

{ #category : #'instance creation' }
CmdCommandMenuItem class >> activatingBy: aMenuCommandActivator [
	^self new 
		activator: aMenuCommandActivator
]

{ #category : #accessing }
CmdCommandMenuItem >> activator [
	^ activator
]

{ #category : #accessing }
CmdCommandMenuItem >> activator: anObject [
	activator := anObject
]

{ #category : #accessing }
CmdCommandMenuItem >> command [
	^ activator command
]

{ #category : #accessing }
CmdCommandMenuItem >> description [
	^activator commandDescription
]

{ #category : #execution }
CmdCommandMenuItem >> executeCommand [
	activator executeCommand
]

{ #category : #accessing }
CmdCommandMenuItem >> findGroupLike: menuGroupClass ifExists: aBlockClosure [ 

]

{ #category : #accessing }
CmdCommandMenuItem >> icon [
	^activator menuItemIcon
]

{ #category : #testing }
CmdCommandMenuItem >> includesCommand: aCommandClass [

	^activator activatesCommand: aCommandClass
]

{ #category : #testing }
CmdCommandMenuItem >> isActive [
	^activator canExecuteCommand
]

{ #category : #testing }
CmdCommandMenuItem >> isCommandItem [
	^true
]

{ #category : #testing }
CmdCommandMenuItem >> isSimilarTo: aMenuItem [
	^self class == aMenuItem class and: [ 
		activator isSimilarTo: aMenuItem activator]
]

{ #category : #accessing }
CmdCommandMenuItem >> name [
	^activator menuItemName
]

{ #category : #accessing }
CmdCommandMenuItem >> order [
	^activator menuItemOrder
]

{ #category : #printing }
CmdCommandMenuItem >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self name.
	aStream nextPut: $).
]

{ #category : #updating }
CmdCommandMenuItem >> synchronizeContentWith: anotherMenuItem [

]
