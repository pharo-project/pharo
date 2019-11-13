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
SpCodeCommand class >> defaultShortcutKey [

	^ nil
]

{ #category : #converting }
SpCodeCommand >> asSpecCommand [
	| command |

	command := super asSpecCommand.
	self shortcutKey 
		ifNotNil: [ :key | command shortcutKey: key ].

	^ command
]

{ #category : #accessing }
SpCodeCommand >> shortcutKey [

	^ self class defaultShortcutKey
]
