"
I am a base command for new tools. 
I define common behavior that all commands can have: 

- maybe an icon
- maybe a shortcut
"
Class {
	#name : #StCommand,
	#superclass : #CmCommand,
	#category : #'NewTools-Core-Command'
}

{ #category : #accessing }
StCommand class >> defaultIconName [

	^ nil
]

{ #category : #accessing }
StCommand class >> defaultShortcut [

	^ nil
]

{ #category : #accessing }
StCommand >> application [

	^ context application
]

{ #category : #converting }
StCommand >> asSpecCommand [ 
	| command |
	
	command := super asSpecCommand 
		iconProvider: self application;
		iconName: self class defaultIconName;
		yourself.
		
	self class defaultShortcut 
		ifNotNil: [ :keyCombination | command shortcutKey: keyCombination ].
		
	^ command
]
