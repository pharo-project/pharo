"
I am a replacement tool for *class* comments. 

I need to be refactored somehow as `ClyPackageRichTextCommentEditorToolMorph` and `ClyRichTextClassCommentEditorToolMorph` are basically identical
"
Class {
	#name : #ClyRichTextClassCommentEditorToolMorph,
	#superclass : #ClyClassCommentEditorToolMorph,
	#instVars : [
		'isRendering'
	],
	#category : #'Microdown-Calypso-UI'
}

{ #category : #activation }
ClyRichTextClassCommentEditorToolMorph class >> classCommentTabActivation [ 
	"Do nothing - part of a hack for overriding the class comment tool in Calypso"
]

{ #category : #activation }
ClyRichTextClassCommentEditorToolMorph class >> classTabActivation [  
	"Do nothing - part of a hack for overriding the class comment tool in Calypso"
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> asRenderedText [
	"Assumes we are in view pillar source mode. editingText does not return a Text in case of working with RGComment. asText turns it into a Text, and asString into a string, which is what we expect. 
	If we have a real source code, asText asString is dummy and does not fail."

	self flag: #fixme. "this is strange to have a super here."
	
	^ PRRichTextComposer renderComment: super editingText asText asString of: self editingClass
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> buildTextMorph [
	super buildTextMorph.
	self setBackgroundColor: self renderColor.
	self bindKeyCombination: $r command toAction: [ self toggleMode ].
	self disable 
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> changesAccepted [
	| res |
	res := super changesAccepted.
	self switchToRenderMode.
	^res
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> disable [
	textMorph enabled: false
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> editColor [
	^ self theme backgroundColor 
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> editingText [
	^ isRendering 
		ifTrue: [ self asRenderedText ]
		ifFalse: [ super editingText ]
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> enable [
	textMorph enabled: true
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> fillStatusBar [

	super fillStatusBar.
	statusBar addCommandItem: (ClyEditCommentSwitchMorph for: self).
	statusBar addCommandItem: (ClySyntaxHelpMorph for: self).

]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> initialize [
	super initialize.
	isRendering := true.

]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> isRendering [
	^ isRendering
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> renderColor [
	| bgColor |
	bgColor := self theme backgroundColor.
	^ bgColor lightness < 0.5
		ifTrue: [ bgColor + (Color r: 0.0 g: 0.04 b:0.08 )]
		ifFalse: [ bgColor darker ]
]

{ #category : #initialization }
ClyRichTextClassCommentEditorToolMorph >> setBackgroundColor: aColor [
	
	textMorph color: aColor
	
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> switchToEditMode [
	isRendering := false.
	self enable.
	self setBackgroundColor: self editColor.
	self triggerUpdate
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> switchToRenderMode [
	self hasUnacceptedEdits
		ifTrue: [ ^ self ].
	isRendering := true.
	self setBackgroundColor: self renderColor.
	self disable.
	self triggerUpdate
]

{ #category : #rendering }
ClyRichTextClassCommentEditorToolMorph >> toggleMode [
	isRendering
		ifTrue: [ self switchToEditMode ]
		ifFalse: [ self switchToRenderMode ]
]
