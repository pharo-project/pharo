"
I am a command allowing to inspect a contact.
"
Class {
	#name : #SpInspectContactCommand,
	#superclass : #SpContactBookCommand,
	#category : #'Spec2-Commander2-ContactBook-Extensions'
}

{ #category : #hooks }
SpInspectContactCommand >> execute [
	self context inspect
]

{ #category : #initialization }
SpInspectContactCommand >> initialize [
	super initialize.
	self
		basicName: 'Inspect';
		basicDescription: 'Inspect the context of this command.'
]
