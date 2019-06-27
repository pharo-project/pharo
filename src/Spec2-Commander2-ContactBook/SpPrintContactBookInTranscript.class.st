Class {
	#name : #SpPrintContactBookInTranscript,
	#superclass : #SpContactBookCommand,
	#category : #'Spec2-Commander2-ContactBook-Spec'
}

{ #category : #hooks }
SpPrintContactBookInTranscript >> execute [
	"Execute the actions that should be done by the command.
	 This method expect that the context has been put in #context inst. var. if any context is relevant."
	Transcript open.
	self contactBook contents do: [ :contact |
		Transcript show: contact name; show: ' - '; show: contact name; cr ]
]

{ #category : #initialization }
SpPrintContactBookInTranscript >> initialize [
	super initialize.
	self
		basicName: 'Print'; "This is the name of the command that will be shown to user."
		basicDescription: 'Print the contact book in Transcript.' "This is the description of the command that will be shown to user."
]
