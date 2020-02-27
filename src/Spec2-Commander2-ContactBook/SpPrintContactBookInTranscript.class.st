Class {
	#name : #SpPrintContactBookInTranscript,
	#superclass : #SpContactBookCommand,
	#category : #'Spec2-Commander2-ContactBook-Spec'
}

{ #category : #executing }
SpPrintContactBookInTranscript >> execute [
	"Execute the actions that should be done by the command.
	 This method expect that the context has been put in #context inst. var. if any context is relevant."

	Smalltalk tools transcript open.
	self contactBook contents do: [ :contact | self traceCr: contact name , ' - ' , contact name ]
]

{ #category : #initialization }
SpPrintContactBookInTranscript >> initialize [
	super initialize.
	self
		name: 'Print';
		"This is the name of the command that will be shown to user."
			description: 'Print the contact book in Transcript.'	"This is the description of the command that will be shown to user."
]
