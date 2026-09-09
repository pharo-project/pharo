"
I represent a nameless command, serving as the root of the command hierarchy and providing default behavior and error handling.
My subcommands are effectively the main commands available to the user.
"
Class {
	#name : 'ClapRoot',
	#superclass : 'ClapCommandSpec',
	#category : 'Clap-Core-Specification',
	#package : 'Clap-Core',
	#tag : 'Specification'
}

{ #category : 'matching - testing' }
ClapRoot >> canMatchWith: word [
	^ true
]

{ #category : 'testing' }
ClapRoot >> isCommand [
	"A clap root is not a true command but the entry point for Clap commands"

	^ false
]

{ #category : 'accessing' }
ClapRoot >> matchClass [
	^ ClapCompositeMatch
]
