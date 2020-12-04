"
I am a replacement for the """"package comments"""" tool.


I need to be refactored somehow as ==ClyPackageRichTextCommentEditorToolMorph== and ==ClyRichTextClassCommentEditorToolMorph== are basically identical
"
Class {
	#name : #ClyPackageRichTextCommentEditorToolMorph,
	#superclass : #ClyPackageCommentEditorToolMorph,
	#instVars : [
		'isRendering'
	],
	#category : #'Microdown-Calypso-UI'
}

{ #category : #activation }
ClyPackageRichTextCommentEditorToolMorph class >> browserTabActivation [  
	"Do nothing - part of a hack for overriding the class package tool in Calypso"
]

{ #category : #activation }
ClyPackageRichTextCommentEditorToolMorph class >> browserTabActivation2 [  
	"Do nothing - part of a hack for overriding the class package tool in Calypso"
]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> asRenderedText [
	self flag: #fixme. "super bad usage"
	^ PRRichTextComposer renderComment: super editingText asText asString of: self editingPackage
]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> buildTextMorph [
	super buildTextMorph.
	self setBackgroundColor: self renderColor.
	self bindKeyCombination: $r command toAction: [ self toggleMode ].
	self disable.
]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> changesAccepted [
	| res |
	res := super changesAccepted.
	self switchToRenderMode.
	^res
]

{ #category : #initialization }
ClyPackageRichTextCommentEditorToolMorph >> disable [
	textMorph enabled: false
]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> editColor [
	^ self theme backgroundColor 
]

{ #category : #initialization }
ClyPackageRichTextCommentEditorToolMorph >> editingText [
	^ isRendering 
		ifTrue: [ self asRenderedText ]
		ifFalse: [ super editingText ]
]

{ #category : #initialization }
ClyPackageRichTextCommentEditorToolMorph >> enable [
	textMorph enabled: true
]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> fillStatusBar [

	super fillStatusBar.
	statusBar addCommandItem: (ClyEditCommentSwitchMorph for: self).
	statusBar addCommandItem: (ClySyntaxHelpMorph for: self).

]

{ #category : #initialization }
ClyPackageRichTextCommentEditorToolMorph >> initialize [
	super initialize.
	isRendering := true.

]

{ #category : #testing }
ClyPackageRichTextCommentEditorToolMorph >> isRendering [
	^ isRendering
]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> renderColor [

	| bgColor |
	bgColor := self theme backgroundColor.
	^ bgColor lightness < 0.5
		ifTrue: [ bgColor + (Color r: 0.0 g: 0.04 b:0.08 )]
		ifFalse: [ bgColor darker ]

]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> setBackgroundColor: aColor [

	textMorph color: aColor
	
]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> switchToEditMode [
	isRendering := false.
	self enable.
	self setBackgroundColor: self editColor.
	self triggerUpdate
]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> switchToRenderMode [
	self hasUnacceptedEdits
		ifTrue: [ ^ self ].
	isRendering := true.
	self setBackgroundColor: self renderColor.
	self disable.
	self triggerUpdate
]

{ #category : #rendering }
ClyPackageRichTextCommentEditorToolMorph >> toggleMode [
	isRendering
		ifTrue: [ self switchToEditMode ]
		ifFalse: [ self switchToRenderMode ]
]
