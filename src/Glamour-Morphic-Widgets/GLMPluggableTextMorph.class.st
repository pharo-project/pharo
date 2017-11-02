"
We subclass the default PluggableTextMorph just to raise the proper announcements when the text or when the selection changes.
"
Class {
	#name : #GLMPluggableTextMorph,
	#superclass : #PluggableTextMorph,
	#instVars : [
		'announcer'
	],
	#category : 'Glamour-Morphic-Widgets'
}

{ #category : #'interactive error protocol' }
GLMPluggableTextMorph >> announceSelection: aString from: start to: stop [
	self announcer announce: (
		GLMSelectedTextChanged new 
			start: start; 
			end: stop;
			selectedText: aString )
]

{ #category : #accessing }
GLMPluggableTextMorph >> announcer [
	^ announcer ifNil: [announcer := GLMAnnouncer new]
]

{ #category : #do-its }
GLMPluggableTextMorph >> evaluateSelectionAndDo: aBlock [

	^ self handleEdit: [textMorph editor evaluateSelectionAndDo: aBlock]
]

{ #category : #private }
GLMPluggableTextMorph >> handleEdit: editBlock [
	"We override this hook method to intercept pasting in the text morph.
	When pasting happens, we update the text port"
	super handleEdit: editBlock.
	self announcer announce: (GLMTextChanged new text: self text)
]

{ #category : #'interactive error protocol' }
GLMPluggableTextMorph >> selectFrom: start to: stop [
	| result |
	result := super selectFrom: start to: stop.
	self announcer announce: (GLMSelectedTextChanged new start: start; end: stop).
	^ result
]

{ #category : #'new selection' }
GLMPluggableTextMorph >> selectLine [

	self handleEdit: [ 
		textMorph editor lineSelectAndEmptyCheck: [^ nil] ].
]

{ #category : #private }
GLMPluggableTextMorph >> textMorphClass [
	"this is needed for intercepting the shortcuts"
	^ GLMTextMorphForEditView
]
