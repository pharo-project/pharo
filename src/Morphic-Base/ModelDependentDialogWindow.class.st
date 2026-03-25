"
DialogWindow that updates content based upon its model.
"
Class {
	#name : 'ModelDependentDialogWindow',
	#superclass : 'DialogWindowMorph',
	#category : 'Morphic-Base-Windows',
	#package : 'Morphic-Base',
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
