"
I am a trait that gathers the common aspects of the package and class comment editors.
"
Trait {
	#name : 'TClyRichTextCommentEditorPaneMorph',
	#instVars : [
		'isRendering'
	],
	#category : 'BeautifulComments-Core',
	#package : 'BeautifulComments',
	#tag : 'Core'
}

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> buildTextMorph [
	super buildTextMorph.
	self setBackgroundColor: self renderColor.
	self bindKeyCombination: $r command toAction: [ self toggleMode ].
	self disable 
]

{ #category : 'class initialization' }
TClyRichTextCommentEditorPaneMorph >> captureErrors [ 
	^ BeautifulComments captureErrors 
]

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> changesAccepted [
	| res |
	res := super changesAccepted.
	self switchToRenderMode.
	^res
]

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> editColor [
	^ self theme backgroundColor 
]

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> editingText [
	| text |
	text := self basicEditingText.
	^ isRendering 
		ifTrue: [ self asRenderedText: text ]
		ifFalse: [ text ]
]

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> initializePaneTrait [ 
	isRendering := true.

]

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> isRendering [

	^ isRendering
]

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> renderColor [
	| bgColor |
	bgColor := self theme backgroundColor.
	^ bgColor lightness < 0.5
		ifTrue: [ bgColor + (Color r: 0.0 g: 0.04 b:0.08 )]
		ifFalse: [ bgColor darker ]
]

{ #category : 'rendering' }
TClyRichTextCommentEditorPaneMorph >> renderComment: aString of: aClassOrPackage [ 
	"The actual implementation is moved to BeautifulComments - basically to allow Testing"
	
	^ BeautifulComments renderComment: aString of: aClassOrPackage
]

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> switchToEditMode [
	isRendering := false.
	self enable.
	self setBackgroundColor: self editColor.
	self triggerUpdate
]

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> switchToRenderMode [
	self hasUnacceptedEdits
		ifTrue: [ ^ self ].
	isRendering := true.
	self setBackgroundColor: self renderColor.
	self disable.
	self triggerUpdate
]

{ #category : 'trait rendering' }
TClyRichTextCommentEditorPaneMorph >> toggleMode [
	isRendering
		ifTrue: [ self switchToEditMode ]
		ifFalse: [ self switchToRenderMode ]
]
