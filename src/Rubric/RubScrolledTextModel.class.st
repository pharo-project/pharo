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
		'textSegments',
		'interactionModel'
	],
	#category : 'Rubric-Editing-Widgets'
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

{ #category : #undo-redo }
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

{ #category : #necompletion-extensions }
RubScrolledTextModel >> guessTypeForName: aString [
	"Packaging notes:
	Since this method is necessary for Ecompletion to work we had the following possibilities:
		- Case 1: add this method as an extension of NECompletion, but it was creating a dependency from NECompletion to 	 		  Rubric.
		- Case 2: add this method in this package. 
		It implies somehow that Rubric depends on NEC but this is a lose coupling, when NECompletion is not loaded, we will get 		some dead code.
		- Case 3: we could create a package NEC-Rubric that depends on Rubric and NECompletion. It seems a bit overkill.
		
	Now it looks like RubSmalltalkEditor could be packaged out of Rubric-Core.
	and it would handle this. 
	In addition the logic completion based on codeCompletionAround:  textMorph:  keyStroke: 
	and the use of the registration as a tool is not really good because the completion requirement is not explicit. 
	A better solution would be that the SmalltalkEditor has a default dummy ecompleter and that we can pass a more 
	advanced one."
		
	^ self interactionModel ifNotNil: [ :im | im guessTypeForName: aString ]
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

{ #category : #necompletion-extensions }
RubScrolledTextModel >> isCodeCompletionAllowed [
	| allowed |
	self reconfigureViewWith: [ :scrolledText | allowed := scrolledText textArea editingMode isCodeCompletionAllowed ].
	^ allowed
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
