Class {
	#name : #SpChangePhoneCommand,
	#superclass : #SpContactBookCommand,
	#instVars : [
		'newPhone'
	],
	#category : #'Spec2-Commander2-ContactBook-Extensions'
}

{ #category : #hooks }
SpChangePhoneCommand >> execute [
	"One should never manipulate UI directly from a commande.
	 Instead, you can use one of the subclass of LtCommandNotification.
	 The UI interactions will be handled by the command decorator.
	 Using this mechanism a command is not dependent on the context in which it is used.
	"
	self selectedContact phone: self context newPhone.
	self contactBookPresenter updateView
]

{ #category : #initialization }
SpChangePhoneCommand >> initialize [
	super initialize.
	self
		name: 'Change phone';
		description: 'Change the phone number of the contact.'
]

{ #category : #accessing }
SpChangePhoneCommand >> newPhone [
	^ newPhone
]

{ #category : #accessing }
SpChangePhoneCommand >> newPhone: anObject [
	newPhone := anObject
]
