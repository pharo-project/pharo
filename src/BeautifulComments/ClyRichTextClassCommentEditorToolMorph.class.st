"
I am a replacement tool for **class** comments. 

I need to be refactored somehow as `ClyPackageRichTextCommentEditorToolMorph` and `ClyRichTextClassCommentEditorToolMorph` are basically identical
"
Class {
	#name : 'ClyRichTextClassCommentEditorToolMorph',
	#superclass : 'ClyClassEditorToolMorph',
	#traits : 'TClyRichTextCommentEditorPaneMorph',
	#classTraits : 'TClyRichTextCommentEditorPaneMorph classTrait',
	#instVars : [
		'isRendering'
	],
	#category : 'BeautifulComments-UI',
	#package : 'BeautifulComments',
	#tag : 'UI'
}

{ #category : 'activation' }
ClyRichTextClassCommentEditorToolMorph class >> classCommentTabActivation [
	<classAnnotation>
	
	^ClyTabActivationStrategyAnnotation for: ClyClassComment asCalypsoItemContext
]

{ #category : 'operations' }
ClyRichTextClassCommentEditorToolMorph >> applyChanges [

	self pendingText asString = self editingText asString ifFalse: [ editingClass comment: self pendingText stamp: self editionTimestamp ].

	^ true
]

{ #category : 'rendering' }
ClyRichTextClassCommentEditorToolMorph >> asRenderedText: comment [
	"Assumes we are in view source mode. commment might be an instance of RGComment. 
	asText turns it into a Text, and asString into a string, which is what we expect. 
	If we have a real source code, asText asString is dummy and does not fail."

	^ self renderComment: comment asText asString of: self editingClass 
]

{ #category : 'accessing' }
ClyRichTextClassCommentEditorToolMorph >> basicEditingText [

	^ editingClass comment
]

{ #category : 'initialization' }
ClyRichTextClassCommentEditorToolMorph >> defaultIconName [
	^(context lastSelectedItem isMarkedWith: ClyNoCommentTag)
		ifTrue: [ #uncommentedClass ]
		ifFalse: [ #comments]
]

{ #category : 'initialization' }
ClyRichTextClassCommentEditorToolMorph >> defaultTitle [
	^'Comment'
]

{ #category : 'rendering' }
ClyRichTextClassCommentEditorToolMorph >> disable [
	textMorph enabled: false
]

{ #category : 'accessing' }
ClyRichTextClassCommentEditorToolMorph >> editingMode [
	^ RubSmalltalkCommentMode new
]

{ #category : 'rendering' }
ClyRichTextClassCommentEditorToolMorph >> enable [
	textMorph enabled: true
]

{ #category : 'rendering' }
ClyRichTextClassCommentEditorToolMorph >> fillStatusBar [

	super fillStatusBar.
	statusBar addCommandItem: (ClyEditCommentSwitchMorph for: self).
	statusBar addCommandItem: (ClySyntaxHelpMorph for: self).

]

{ #category : 'initialization' }
ClyRichTextClassCommentEditorToolMorph >> initialize [

	super initialize.
	self initializePaneTrait
]

{ #category : 'printing' }
ClyRichTextClassCommentEditorToolMorph >> printContext [
	^super printContext , ' / ', editingClass name
]

{ #category : 'initialization' }
ClyRichTextClassCommentEditorToolMorph >> setBackgroundColor: aColor [
	
	textMorph color: aColor
	
]

{ #category : 'initialization' }
ClyRichTextClassCommentEditorToolMorph >> setUpDefaultIcon [
	"Here we duplicate logic of #defaultIconName to directly ask class about comment.
	This method is called during background tab building. 
	So it does not really affect performance which is critical for remote scenario"
	
	| iconName |
	iconName := editingClass hasComment ifTrue: [ #comments ] ifFalse: [ #uncommentedClass ].
	containerTab icon: (self iconNamed: iconName)
]

{ #category : 'testing' }
ClyRichTextClassCommentEditorToolMorph >> wantsTextStyling [
	^false
]
