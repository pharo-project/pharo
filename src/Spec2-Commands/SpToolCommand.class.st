"
I'm a base command to be used on spec 2 presenters. 
My children will define actions that can be executed as part of the Pharo System. 

"
Class {
	#name : #SpToolCommand,
	#superclass : #CmCommand,
	#category : #'Spec2-Commands-Presenters'
}

{ #category : #defaults }
SpToolCommand class >> defaultIconName [
	^nil
]

{ #category : #defaults }
SpToolCommand class >> defaultShortcutKey [

	^ nil
]

{ #category : #default }
SpToolCommand class >> shortName [
	"For menus some commands can implement this helper to provide shorter names.
	Example: default name = 'Browse Command', short name = 'command'.
	It is intended to be accessed through a menu 'browse' then  'command' instead of directly 'browse command'"

	^ self defaultName
]

{ #category : #converting }
SpToolCommand >> asSpecCommand [
	| command |
	command := super asSpecCommand.
	self shortcutKey ifNotNil: [ :key | command shortcutKey: key ].
	self defaultIconName
		ifNotNil: [ :iconName | command iconName: iconName ].
	^ command
]

{ #category : #accessing }
SpToolCommand >> defaultIconName [
	^self class defaultIconName
]

{ #category : #accessing }
SpToolCommand >> shortcutKey [

	^ self class defaultShortcutKey
]
