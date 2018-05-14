"
Common superclass for Monticello tools

Placeholder for a MCVersionCache for MCLazyVersionInfo
"
Class {
	#name : #MCTool,
	#superclass : #Model,
	#instVars : [
		'morph',
		'label',
		'modal',
		'modalValue'
	],
	#classVars : [
		'MCVersionCache'
	],
	#category : #MonticelloGUI
}

{ #category : #'class initialization' }
MCTool class >> flushMCVersionCache [
	MCVersionCache := nil.
	Smalltalk garbageCollect
]

{ #category : #'class initialization' }
MCTool class >> initMCVersionCache [
	MCVersionCache := nil
]

{ #category : #accessing }
MCTool class >> mcVersionCache [
	^ MCVersionCache
		ifNil: [ MCVersionCache := LRUCache new maximumWeight: 32 ]
]

{ #category : #utilities }
MCTool class >> newRepository [
	"Create a new monticello repository"
	| types index |
	types := MCRepository allConcreteSubclasses asArray.
	index := UIManager default chooseFrom: (types collect: [:ea | ea description])
				title: 'Repository type:'.
	^ index = 0 ifFalse: [(types at: index) morphicConfigure]
]

{ #category : #'window color' }
MCTool class >> theme [
	"Answer the ui theme that provides controls."

	^ Smalltalk ui theme 
]

{ #category : #'morphic ui' }
MCTool >> accept [
	" do nothing by default"
]

{ #category : #utilities }
MCTool >> allManagers [

	^ MCWorkingCopy allManagers 
]

{ #category : #'morphic ui' }
MCTool >> answer: anObject [
	modalValue := anObject.
	self close.
]

{ #category : #'morphic ui' }
MCTool >> arrowKey: aCharacter from: aPluggableListMorph [ 
	"backstop"
]

{ #category : #'morphic ui' }
MCTool >> buildWindow [
	| window |
	window := SystemWindow labelled: self label.
	window model: self.
	self widgetSpecs do: [:spec |
		| send fractions offsets |
		send := spec first.
		fractions := spec at: 2 ifAbsent: [#(0 0 1 1)].
		offsets := spec at: 3 ifAbsent: [#(0 0 0 0)].
		window
			addMorph: (self perform: send first withArguments: send allButFirst)
			fullFrame:
				(LayoutFrame new 
					leftFraction: fractions first; 
					topFraction: fractions second;
					rightFraction: fractions third ; 
					bottomFraction: fractions fourth; 
					leftOffset: offsets first;
					topOffset: offsets second;
					rightOffset: offsets third;
					bottomOffset: offsets fourth)].
	^ window
]

{ #category : #'morphic ui' }
MCTool >> buttonEnabled [
	^ true
]

{ #category : #'morphic ui' }
MCTool >> buttonRow [
	^ self buttonRow: self buttonSpecs
]

{ #category : #'morphic ui' }
MCTool >> buttonRow: specArray [
	| aRow |
	aRow := PanelMorph new.
	aRow layoutPolicy: TableLayout new; listDirection: #leftToRight.
	aRow hResizing: #spaceFill; vResizing: #spaceFill; rubberBandCells: true.
	aRow clipSubmorphs: true; borderWidth: 0.
	aRow layoutInset: 2@2; cellInset: 1.
	aRow wrapCentering: #center; cellPositioning: #leftCenter.
	specArray do:
		[:triplet | | aButton state |
			state := triplet at: 5 ifAbsent: [#buttonState].
			aButton := PluggableButtonMorph
				on: self
				getState: state
				action: #performButtonAction:enabled:.
			aButton
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				label: triplet first asString;
				getEnabledSelector: (triplet at: 4 ifAbsent: [#buttonEnabled]);
				arguments: (Array with: triplet second with: (triplet at: 4 ifAbsent: [#buttonEnabled])).
			aRow addMorphBack: aButton.
			aButton setBalloonText: triplet third].
	^ aRow
]

{ #category : #'morphic ui' }
MCTool >> buttonSelected [
	^ false
]

{ #category : #'morphic ui' }
MCTool >> buttonSpecs [
	^ #()
]

{ #category : #'morphic ui' }
MCTool >> buttonState [
	^ false
]

{ #category : #'morphic ui' }
MCTool >> close [
	self window delete
]

{ #category : #'morphic ui' }
MCTool >> defaultAnnotationPaneHeight [ 
	"Answer the receiver's preferred default height for new annotation panes."
	^ 25
]

{ #category : #'morphic ui' }
MCTool >> fillMenu: aMenu fromSpecs: anArray [
	anArray do:
		[:spec |
		aMenu
			add: spec first
			target: self
			selector: spec second
			argumentList: (spec allButFirst: 2)].
	^ aMenu
]

{ #category : #'morphic ui' }
MCTool >> findListMorph: aSymbol [
	^ morph submorphs detect: [:ea | (ea respondsTo: #getListSelector) and: [ea getListSelector = aSymbol]] ifNone: []
]

{ #category : #'morphic ui' }
MCTool >> findTextMorph: aSymbol [
	^ morph submorphs detect: [:ea | (ea respondsTo: #getTextSelector) and: [ea getTextSelector = aSymbol]] ifNone: []
]

{ #category : #'morphic ui' }
MCTool >> getMenu: aMenu [
	^aMenu
]

{ #category : #'morphic ui' }
MCTool >> initialExtent [
	^ 500@500
]

{ #category : #utilities }
MCTool >> interactionModel [
	^ self
]

{ #category : #'morphic ui' }
MCTool >> label [
	^ label ifNil: [self defaultLabel]
]

{ #category : #'morphic ui' }
MCTool >> label: aString [
	label := aString
]

{ #category : #'morphic ui' }
MCTool >> listMorph: listSymbol [
	^ self
		listMorph: (listSymbol, 'List') asSymbol
		selection: (listSymbol, 'Selection') asSymbol
		menu: (listSymbol, 'ListMenu:') asSymbol
]

{ #category : #'morphic ui' }
MCTool >> listMorph: listSymbol keystroke: keystrokeSymbol [
	^ (self
		listMorph: (listSymbol, 'List') asSymbol
		selection: (listSymbol, 'Selection') asSymbol
		menu: (listSymbol, 'ListMenu:') asSymbol)
		keystrokeActionSelector: keystrokeSymbol;
		yourself
]

{ #category : #'morphic ui' }
MCTool >> listMorph: listSymbol selection: selectionSymbol [
	^ PluggableListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
]

{ #category : #'morphic ui' }
MCTool >> listMorph: listSymbol selection: selectionSymbol menu: menuSymbol [
	^ PluggableListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
		menu: menuSymbol
]

{ #category : #'morphic ui' }
MCTool >> listMorph: listSymbol selection: selectionSymbol menu: menuSymbol icon: iconSelector [
	^ (PluggableIconListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
		menu: menuSymbol)
			getIconSelector: iconSelector;
			yourself
]

{ #category : #'morphic ui' }
MCTool >> listMorph: listSymbol selection: selectionSymbol menu: menuSymbol keystroke: keystrokeSymbol [
	^ (PluggableListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
		menu: menuSymbol)
		keystrokeActionSelector: keystrokeSymbol;
		yourself
]

{ #category : #'morphic ui' }
MCTool >> menu [
	" returns nil to let the editing mode offer the right menu"
	^ nil
]

{ #category : #accessing }
MCTool >> minimumExtent [
	"Answer the minumum extent for the tool."
	
	^100@100
]

{ #category : #'morphic ui' }
MCTool >> multiListMorph: listSymbol selection: selectionSymbol listSelection: listSelectionSymbol menu: menuSymbol [
	^ PluggableListMorph
		on: self
		list: listSymbol
		primarySelection: selectionSymbol
		changePrimarySelection: (selectionSymbol, ':') asSymbol
		listSelection: listSelectionSymbol
		changeListSelection: (listSelectionSymbol, 'put:') asSymbol
		menu: menuSymbol
]

{ #category : #'morphic ui' }
MCTool >> performButtonAction: anActionSelector enabled: anEnabledSelector [
	(self perform: anEnabledSelector) 
		ifTrue: [ self perform: anActionSelector ]
]

{ #category : #'morphic ui' }
MCTool >> preferredColor [  
	^ (Color r: 0.627 g: 0.69 b: 0.976)
]

{ #category : #'morphic ui' }
MCTool >> setDefaultFocus [
	"set the default focus on the morph elements.
	specializ this in subclasses"
]

{ #category : #'morphic ui' }
MCTool >> shoutAboutToStyle: aPluggableShoutMorphOrView [
	^ false
]

{ #category : #opening }
MCTool >> show [
	"Open the tool returning the window."

	modal := false.
	self window openInWorld.
	self setDefaultFocus.
	^ self window
]

{ #category : #opening }
MCTool >> showLabelled: labelString [
	
	modal := false.
	self label: labelString.
	self window openInWorld.
	self setDefaultFocus.
	^ self window.
]

{ #category : #'morphic ui' }
MCTool >> showModally [
	modal := true.
	self window openInWorldExtent: 400 @ 400.
	self setDefaultFocus.
	[ self window world notNil ] whileTrue: [ self window outermostWorldMorph doOneCycle ].
	morph := nil.
	^ modalValue
]

{ #category : #'morphic ui' }
MCTool >> summary: aString [
	" do nothing by default"
]

{ #category : #'morphic ui' }
MCTool >> textMorph: aSymbol [
	| textMorph |
	textMorph := RubPluggableTextMorph new
		getTextSelector: aSymbol;
		setTextSelector: (aSymbol , ':') asSymbol;
		on: self;
		beWrapped;
		hScrollbarShowNever;
		beForSmalltalkScripting;
		yourself.
	textMorph announcer when: RubTextAcceptRequest send: #accept to: self.
	textMorph hasUnacceptedEdits: false.
	^ textMorph
]

{ #category : #'morphic ui' }
MCTool >> treeMorph: listSymbol [
	^ self
		treeMorph: (listSymbol, 'Tree') asSymbol
		selection: (listSymbol, 'SelectionWrapper') asSymbol
		menu: (listSymbol, 'TreeMenu:') asSymbol
]

{ #category : #'morphic ui' }
MCTool >> treeMorph: listSymbol selection: selectionSymbol menu: menuSymbol [
	^ SimpleHierarchicalListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
		menu: menuSymbol
		keystroke: nil
]

{ #category : #'morphic ui' }
MCTool >> widgetSpecs [
	^ #()
]

{ #category : #'morphic ui' }
MCTool >> window [
	^ morph ifNil: [morph := self buildWindow]
]
