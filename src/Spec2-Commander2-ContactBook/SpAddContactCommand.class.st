"
I am a command allowing to add a contact.
"
Class {
	#name : #SpAddContactCommand,
	#superclass : #SpContactBookCommand,
	#category : #'Spec2-Commander2-ContactBook-Commands'
}

{ #category : #converting }
SpAddContactCommand >> asSpecCommand [
	"Here we define the additional information the command will need to know
	 when used in the Spec framework.
	"
	^ super asSpecCommand
		shortcutKey: $n meta;
		yourself
]

{ #category : #hooks }
SpAddContactCommand >> execute [
	"One should never manipulate UI directly from a command.
	 Instead, you can use one of the subclasses of LtCommandNotification.
	 The UI interactions will be handled by the command decorator.
	 By using this mechanism, a command is not dependent on the context in which it is used.
	"
	| contact |
	contact := self context newContact.
	self hasSelectedContact
		ifTrue: [ self contactBook addContact: contact after: self selectedContact ]
		ifFalse: [ self contactBook addContact: contact ].
		
	self contactBookPresenter updateView
]

{ #category : #initialization }
SpAddContactCommand >> initialize [
	super initialize.
	self
		name: 'New contact';
		"This is the name of the command that will be shown to the user."
			description: 'Creates a new contact and add it to the contact book.'	"This is the description of the command that will be shown to the user."
]
