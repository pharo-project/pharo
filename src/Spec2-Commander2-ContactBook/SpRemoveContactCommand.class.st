"
I am a command allowing to remove a contact.
"
Class {
	#name : #SpRemoveContactCommand,
	#superclass : #SpContactBookCommand,
	#category : #'Spec2-Commander2-ContactBook-Commands'
}

{ #category : #converting }
SpRemoveContactCommand >> asSpecCommand [
	"Here we define the additional information the command will need to know
	 when used in the Spec framework.
	"
	^ super asSpecCommand
		iconName: #removeIcon;
		shortcutKey: $x meta;
		yourself
]

{ #category : #hooks }
SpRemoveContactCommand >> canBeExecuted [
	^ self context isContactSelected
]

{ #category : #hooks }
SpRemoveContactCommand >> execute [
	self contactBookPresenter removeContact: self selectedContact.
	self contactBookPresenter updateView
]

{ #category : #initialization }
SpRemoveContactCommand >> initialize [
	super initialize.
	self
		name: 'Remove';
		"This is the name of the command that will be shown to the user."
			description: 'Removes the selected contact from the contact book.'	"This is the description of the command that will be shown to the user."
]
