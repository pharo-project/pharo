Class {
	#name : #GLMMorphicListingRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'treeMorph',
		'treeModel'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicListingRenderer >> actOnContextChanged: anAnnouncement [ 
	anAnnouncement property = #selection ifTrue: [ 
		treeModel announcer 
			suspendAll: self suspendedAnnouncements
			while: [
				treeMorph model explicitSelection: anAnnouncement value ] ] 
]

{ #category : #rendering }
GLMMorphicListingRenderer >> actOnUpdatedPresentation: anAnnouncement [
	treeMorph model updateRoots.
	self flag: 'Calling update list explicitly here is a hack. This should be solvable by the model through a notification mechanism'.
	treeMorph updateList
]

{ #category : #private }
GLMMorphicListingRenderer >> newTreeMorph [

	^ GLMPaginatedMorphTreeMorph new
]

{ #category : #rendering }
GLMMorphicListingRenderer >> render: aPresentation [
	| container textInput |
	treeModel := GLMTreeMorphModel new glamourPresentation: aPresentation.
	container := GLMMorphic containerMorph.
	treeMorph := self treeMorphFor: treeModel and: aPresentation.
	aPresentation allowsInput
		ifTrue: [ 
			textInput := self textInputFor: treeModel withHelpMessage: aPresentation helpMessage.
			treeMorph layoutFrame bottomOffset: -26.
			container addMorphBack: textInput ].
	container addMorphBack: treeMorph.
	self installActionsOnModel: treeModel fromPresentation: aPresentation.
	self installKeystrokeActionsOnMorph: treeMorph fromPresentation: aPresentation.
	aPresentation selection notNil
		ifTrue: [ 
			treeModel announcer
				suspendAll: self suspendedAnnouncements
				while: [ treeMorph model explicitSelection: aPresentation selection ] ].	"When the morph changes, we want to update the glamour model"
	treeModel announcer
		when: GLMTreeMorphSelectionChanged
		do: [ :ann | 
			aPresentation announcer
				suspendAllWhile: [ 
					aPresentation selection: ann selectionValue.
					aPresentation selectionPath: ann selectionPathValue ] ].
	treeModel announcer
		when: GLMTreeMorphStrongSelectionChanged
		do: [ :ann | aPresentation strongSelection: ann strongSelectionValue ].	"When the glamour model changes, we want to update the morph"
	aPresentation when: GLMContextChanged send: #actOnContextChanged: to: self.
	aPresentation when: GLMPresentationUpdated send: #actOnUpdatedPresentation: to: self.
	^ container
]

{ #category : #private }
GLMMorphicListingRenderer >> suspendedAnnouncements [
	^ OrderedCollection  
		with: GLMTreeMorphSelectionChanged 
		with: GLMTreeMorphStrongSelectionChanged 
		with: GLMMenuItemSelected 
		with: GLMKeyStroke
]

{ #category : #private }
GLMMorphicListingRenderer >> textInputFor: aTreeModel withHelpMessage: helpStringMessage [
	| textInput |
	textInput := self theme
		newTextEntryIn: nil
		for: aTreeModel
		get: #inputText
		set: #inputText:
		class: String
		getEnabled: #inputTextEnabled
		help: helpStringMessage.
	textInput askBeforeDiscardingEdits: false.
	textInput ghostText: helpStringMessage.
	textInput layoutFrame: (LayoutFrame identity 
			topFraction: 1;
			topOffset: -26;
			bottomOffset: -4).
	^ textInput
]

{ #category : #private }
GLMMorphicListingRenderer >> treeMorphFor: tmpTreeModel and: aPresentation [
	| tmpTreeMorph columns |
	tmpTreeMorph := self newTreeMorph.
	tmpTreeMorph
		makeLastColumnUnbounded;
		doubleClickSelector: #onDoubleClick;
		getMenuSelector: #menu:shifted:;
		keystrokeActionSelector: #keyStroke:from:;
		columnInset: 3;
		rowInset: 1;
		cornerStyle: tmpTreeMorph preferredCornerStyle;
		borderStyle: (BorderStyle inset width: 1);
		autoDeselection: aPresentation allowsDeselection;
		dragEnabled: tmpTreeModel dragEnabled;
		dropEnabled: tmpTreeModel dropEnabled;		
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		layoutFrame: (LayoutFrame identity).
	columns := aPresentation columns isEmpty 
		ifTrue: [ OrderedCollection with: (MorphTreeColumn new rowMorphGetSelector: #elementColumn)]
		ifFalse: [
			aPresentation columns collect: [:each | 
				GLMMorphTreeColumn new 
					startWidth: each width;
					glamourColumn: each; 
					headerButtonLabel: (aPresentation titleValueOfColumn: each)
					font: StandardFonts menuFont 
					target: tmpTreeModel
					actionSelector: #sortByColumn:
					arguments: {each}; 
					yourself ].
		].
	tmpTreeMorph
		preferedPaneColor: Smalltalk ui theme backgroundColor;	
		model: tmpTreeModel;
		columns: columns.
	aPresentation isMultiple 
		ifTrue: [tmpTreeMorph beMultiple] 
		ifFalse: [tmpTreeMorph beSingle].
	tmpTreeMorph chunkSize: aPresentation amountToShow.
	tmpTreeMorph pageSize: aPresentation amountToShow.
	tmpTreeMorph vShowScrollBar.
	tmpTreeMorph hShowScrollBar.
	^ tmpTreeMorph buildContents
]
