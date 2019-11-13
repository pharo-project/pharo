"
I am the main model of a text area. I should be used to dialog with the container of the text area. I am able to communicate the necessary infos to the morph. 
"
Class {
	#name : #RubScrolledTextModel,
	#superclass : #Object,
	#traits : 'TViewModel',
	#classTraits : 'TViewModel classTrait',
	#instVars : [
		'hasUnacceptedEdits',
		'announcer',
		'text',
		'primarySelectionInterval',
		'interactionModel'
	],
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #'text managing' }
RubScrolledTextModel >> acceptEdits [
	"Accept the current contents and end editing."
	self announce: RubTextAcceptedInModel new.
	self clearUserEdits 
]

{ #category : #'text managing' }
RubScrolledTextModel >> acceptEditsInView [
	self reconfigureViewWith:  [ :view | 
		view textArea announce: (RubTextAcceptRequest morph: view textArea) ]
]

{ #category : #accessing }
RubScrolledTextModel >> announce: anAnnouncement [ 
	self announcer announce: anAnnouncement
]

{ #category : #accessing }
RubScrolledTextModel >> announcer [
	^ announcer ifNil: [ announcer := self defaultAnnouncer ]
]

{ #category : #binding }
RubScrolledTextModel >> bindingOf: aString [ 
	^ interactionModel ifNotNil:  [ interactionModel bindingOf: aString  ]
]

{ #category : #'text managing' }
RubScrolledTextModel >> canDiscardEdits [
	^ self hasUnacceptedEdits not
]

{ #category : #'text managing' }
RubScrolledTextModel >> cancelEdits [
	"Cancel the current contents and end editing."
	self announce: RubCancelEditRequestedInModel new.
	self clearUserEdits  
]

{ #category : #'undo-redo' }
RubScrolledTextModel >> clearUndoManager [
	self reconfigureViewWith: [ :view | view textArea editingState clearUndoManager: nil ]
]

{ #category : #'text managing' }
RubScrolledTextModel >> clearUserEdits [
	self hasUnacceptedEdits: false
]

{ #category : #accessing }
RubScrolledTextModel >> defaultAnnouncer [
	^ self defaultAnnouncerClass new
]

{ #category : #accessing }
RubScrolledTextModel >> defaultAnnouncerClass [ 
	^Announcer
]

{ #category : #accessing }
RubScrolledTextModel >> doItContext [
	^ interactionModel ifNotNil: [ :im | im doItContext ]
]

{ #category : #accessing }
RubScrolledTextModel >> doItReceiver [
	^ interactionModel ifNotNil: [ :im | im doItReceiver ]
]

{ #category : #formatting }
RubScrolledTextModel >> formatSourceCode [
	self interactionModel formatSourceCode .
]

{ #category : #'view updating' }
RubScrolledTextModel >> formatSourceCodeInView [
	self reconfigureViewWith: [ :view | view formatSourceCode ]
]

{ #category : #'text managing' }
RubScrolledTextModel >> getString [
	^ self getText asString
]

{ #category : #'text managing' }
RubScrolledTextModel >> getText [
	^ self text
]

{ #category : #shout }
RubScrolledTextModel >> hasBindingOf: aString [
	^ interactionModel ifNil: [ false ] ifNotNil: [ interactionModel hasBindingOf: aString ]
]

{ #category : #shout }
RubScrolledTextModel >> hasBindingThatBeginsWith: aString [ 
	^ interactionModel ifNil: [ false ] ifNotNil:  [ interactionModel hasBindingThatBeginsWith: aString  ]
]

{ #category : #accessing }
RubScrolledTextModel >> hasUnacceptedEdits [
	^ hasUnacceptedEdits ifNil: [ hasUnacceptedEdits := false ]
]

{ #category : #'text managing' }
RubScrolledTextModel >> hasUnacceptedEdits: aBoolean [
	hasUnacceptedEdits := aBoolean.
	self announce: RubEditsStateChangedInModel new
]

{ #category : #accessing }
RubScrolledTextModel >> interactionModel [
	^ interactionModel
]

{ #category : #accessing }
RubScrolledTextModel >> interactionModel: anObject [ 	
	interactionModel := anObject
]

{ #category : #menu }
RubScrolledTextModel >> menu [
	^ nil
]

{ #category : #'view creating' }
RubScrolledTextModel >> newScrolledText [
	^ (RubScrolledTextMorph on: self) getSelectionSelector: #primarySelectionInterval; yourself
]

{ #category : #'view creating' }
RubScrolledTextModel >> newTextField [
	^ (RubTextFieldMorph on: self) getSelectionSelector: #primarySelectionInterval; yourself
]

{ #category : #'text managing' }
RubScrolledTextModel >> primarySelectionInterval [
	^ primarySelectionInterval 
]

{ #category : #'view updating' }
RubScrolledTextModel >> reconfigureViewWith: aBlockWithAScrolledTextMorphAsArgument [
	self announce: (RubConfigurationChange new configurationBlock: aBlockWithAScrolledTextMorphAsArgument)
]

{ #category : #accessing }
RubScrolledTextModel >> selectedClassOrMetaClass [
	^ interactionModel ifNotNil: [ :im | im selectedClassOrMetaClass ]
]

{ #category : #'text managing' }
RubScrolledTextModel >> setInitialText: aText [ 
	self setText:  aText.
	self clearUserEdits.
	^ true
]

{ #category : #'text managing' }
RubScrolledTextModel >> setPrimarySelectionInterval: anInterval [

	primarySelectionInterval := anInterval.
	self announce: RubPrimarySelectionUpdatedInModel
]

{ #category : #'text managing' }
RubScrolledTextModel >> setText: aText [ 
	self text:  aText.
	self announcer announce: RubTextUpdatedInModel.
	^ true
]

{ #category : #'text managing' }
RubScrolledTextModel >> setText: aText from: aView [
	^ self setText: aText
]

{ #category : #menu }
RubScrolledTextModel >> shiftMenu [
	^ nil
]

{ #category : #'text managing' }
RubScrolledTextModel >> text [

	^ text ifNil: [ text := '' asText ]
]

{ #category : #'text managing' }
RubScrolledTextModel >> text: atextOrString [
	text := atextOrString asText
]
