"
DialogWindow that updates content based upon its model.
"
Class {
	#name : 'ModelDependentDialogWindow',
	#superclass : 'DialogWindowMorph',
	#category : 'Polymorph-Widgets-Windows',
	#package : 'Polymorph-Widgets',
	#tag : 'Windows'
}

{ #category : 'adding' }
ModelDependentDialogWindow >> addInitialPanel [
	"Don't until the model is set."
]

{ #category : 'adding' }
ModelDependentDialogWindow >> model: anObject [
	"Set the model and add the panel for it."

	super model: anObject.
	self paneMorphs copy do: [:p | p delete].
	self addMainPanel
]
