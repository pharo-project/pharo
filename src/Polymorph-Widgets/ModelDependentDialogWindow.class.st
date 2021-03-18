"
DialogWindow that updates content based upon its model.
"
Class {
	#name : #ModelDependentDialogWindow,
	#superclass : #DialogWindow,
	#category : #'Polymorph-Widgets-Windows'
}

{ #category : #accessing }
ModelDependentDialogWindow >> addInitialPanel [
	"Don't until the model is set."
]

{ #category : #accessing }
ModelDependentDialogWindow >> model: anObject [
	"Set the model and add the panel for it."
	
	super model: anObject.
	self paneMorphs copy do: [:p | p delete].
	self addMainPanel
]
