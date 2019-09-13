"
I am a command allowing to rename a contact.
"
Class {
	#name : #SpRenameContactCommand,
	#superclass : #SpContactBookCommand,
	#category : #'Spec2-Commander2-ContactBook-Commands'
}

{ #category : #converting }
SpRenameContactCommand >> asSpecCommand [
	"Here we define the additional information the command will need to know
	 when used in the Spec framework.
	"
	^ super asSpecCommand
		shortcutKey: $r meta;
		yourself
]

{ #category : #hooks }
SpRenameContactCommand >> canBeExecuted [
	^ self context isContactSelected
]

{ #category : #hooks }
SpRenameContactCommand >> execute [
	self selectedContact name: self context newName.
	self contactBookPresenter updateView
]

{ #category : #initialization }
SpRenameContactCommand >> initialize [
	super initialize.
	self
		name: 'Rename';
		"This is the name of the command that will be shown to the user."
			description: 'Rename the selected contact.'	"This is the description of the command that will be shown to the user."
]
