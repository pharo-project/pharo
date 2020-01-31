"
I'm a base command to be used on code presenter. 
My children will define actions that can be executed as part of the Pharo System. 
Operations like ""do it"", ""print it"" will be implemented by extending me. 
"
Class {
	#name : #SpCodeCommand,
	#superclass : #CmCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeCommand class >> defaultIconName [
	^nil
]

{ #category : #defaults }
SpCodeCommand class >> defaultShortcutKey [

	^ nil
]

{ #category : #converting }
SpCodeCommand >> asSpecCommand [
	| command |
	command := super asSpecCommand.
	self shortcutKey ifNotNil: [ :key | command shortcutKey: key ].
	self defaultIconName
		ifNotNil: [ :iconName | command iconName: iconName ].
	^ command
]

{ #category : #accessing }
SpCodeCommand >> defaultIconName [
	^self class defaultIconName
]

{ #category : #accessing }
SpCodeCommand >> shortcutKey [

	^ self class defaultShortcutKey
]
