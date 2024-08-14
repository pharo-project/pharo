"
I am a replacement for the _package comments_ tool.


"
Class {
	#name : 'ClyPackageRichTextCommentEditorToolMorph',
	#superclass : 'ClyTextEditorToolMorph',
	#traits : 'TClyRichTextCommentEditorPaneMorph',
	#classTraits : 'TClyRichTextCommentEditorPaneMorph classTrait',
	#instVars : [
		'editingPackage',
		'isRendering'
	],
	#category : 'BeautifulComments-Core',
	#package : 'BeautifulComments',
	#tag : 'Core'
}

{ #category : 'activation' }
ClyPackageRichTextCommentEditorToolMorph class >> browserTabActivation [
	"This declaration specifies that in any browser when packages are selected, a comment editor will be available in a tab."

	<classAnnotation>
	^ClyTabActivationStrategyAnnotation for: Package asCalypsoItemContext
]

{ #category : 'activation' }
ClyPackageRichTextCommentEditorToolMorph class >> browserTabActivation2 [
	<classAnnotation>
	
	^ClyTabActivationStrategyAnnotation for: ClyClassGroup asCalypsoItemContext
]

{ #category : 'testing' }
ClyPackageRichTextCommentEditorToolMorph class >> shouldBeActivatedInContext: aBrowserContext [
	^aBrowserContext isPackageSelected and: [aBrowserContext isClassSelected not]
]

{ #category : 'activation' }
ClyPackageRichTextCommentEditorToolMorph class >> tabOrder [
	^ -10
]

{ #category : 'operations' }
ClyPackageRichTextCommentEditorToolMorph >> applyChanges [

	self pendingText asString = self editingText ifFalse: [ editingPackage packageManifest comment: self pendingText stamp: self editionTimestamp ].

	^ true
]

{ #category : 'rendering' }
ClyPackageRichTextCommentEditorToolMorph >> asRenderedText [

	^ MicRichTextComposer new 
			renderComment: super editingText asText asString 
			of: self editingPackage
]

{ #category : 'rendering' }
ClyPackageRichTextCommentEditorToolMorph >> asRenderedText: comment [
	"Assumes we are in view source mode. commment might be an instance of RGComment. 
	asText turns it into a Text, and asString into a string, which is what we expect. 
	If we have a real source code, asText asString is dummy and does not fail."

	^ self renderComment: comment asText asString of: self editingPackage 
]

{ #category : 'controlling' }
ClyPackageRichTextCommentEditorToolMorph >> attachToSystem [

	browser system 
		when: (ClyPackageChange of: editingPackage) send: #triggerUpdate to: self
]

{ #category : 'accessing' }
ClyPackageRichTextCommentEditorToolMorph >> basicEditingText [

	^ editingPackage packageManifestOrNil 
		ifNil: [ self packageCommentBlank ]
		ifNotNil: [ :manifest | 
			manifest hasComment 
				ifTrue: [ manifest comment ] 
				ifFalse: [ self packageCommentBlank ] ]
]

{ #category : 'initialization' }
ClyPackageRichTextCommentEditorToolMorph >> defaultIconName [
	^#comments
]

{ #category : 'initialization' }
ClyPackageRichTextCommentEditorToolMorph >> defaultTitle [
	^'Comment'
]

{ #category : 'controlling' }
ClyPackageRichTextCommentEditorToolMorph >> detachFromSystem [

	browser system unsubscribe: self
]

{ #category : 'initialization' }
ClyPackageRichTextCommentEditorToolMorph >> disable [
	textMorph enabled: false
]

{ #category : 'accessing' }
ClyPackageRichTextCommentEditorToolMorph >> editingPackage [
	^ editingPackage
]

{ #category : 'accessing' }
ClyPackageRichTextCommentEditorToolMorph >> editingPackage: anObject [
	editingPackage := anObject
]

{ #category : 'initialization' }
ClyPackageRichTextCommentEditorToolMorph >> enable [
	textMorph enabled: true
]

{ #category : 'rendering' }
ClyPackageRichTextCommentEditorToolMorph >> fillStatusBar [

	super fillStatusBar.
	statusBar addCommandItem: (ClyEditCommentSwitchMorph for: self).
	statusBar addCommandItem: (ClySyntaxHelpMorph for: self).

]

{ #category : 'initialization' }
ClyPackageRichTextCommentEditorToolMorph >> initialize [
	super initialize.
	self initializePaneTrait.

]

{ #category : 'testing' }
ClyPackageRichTextCommentEditorToolMorph >> isSimilarTo: anotherBrowserTool [
	(super isSimilarTo: anotherBrowserTool) ifFalse: [ ^false ].
	
	^editingPackage = anotherBrowserTool editingPackage
]

{ #category : 'testing' }
ClyPackageRichTextCommentEditorToolMorph >> isValidInContext: aClyFullBrowserContext [
	
	self context class = aClyFullBrowserContext class 
		ifFalse: [ ^ false ].
		
	^ aClyFullBrowserContext isPackageSelected
		ifTrue: [ self editingPackage = aClyFullBrowserContext lastSelectedPackage ]
		ifFalse: [ false ]
]

{ #category : 'accessing' }
ClyPackageRichTextCommentEditorToolMorph >> packageCommentBlank [

	^'Please comment package here'
]

{ #category : 'printing' }
ClyPackageRichTextCommentEditorToolMorph >> printContext [
	^editingPackage name
]

{ #category : 'rendering' }
ClyPackageRichTextCommentEditorToolMorph >> setBackgroundColor: aColor [

	textMorph color: aColor
	
]

{ #category : 'initialization' }
ClyPackageRichTextCommentEditorToolMorph >> setUpModelFromContext [
	super setUpModelFromContext.

	editingPackage := context lastSelectedPackage
]

{ #category : 'testing' }
ClyPackageRichTextCommentEditorToolMorph >> wantsTextStyling [
	^false
]
