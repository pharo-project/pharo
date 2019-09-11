"
I represent a nameless command, serving as the root of the command hierarchy and providing default behavior and error handling.
My subcommands are effectively the main commands available to the user.
"
Class {
	#name : #ClapRoot,
	#superclass : #ClapCommand,
	#category : #'Clap-Core-Specification'
}

{ #category : #'matching - testing' }
ClapRoot >> canMatchWith: word [
	^ true
]

{ #category : #accessing }
ClapRoot >> matchClass [
	^ ClapCompositeMatch
]
