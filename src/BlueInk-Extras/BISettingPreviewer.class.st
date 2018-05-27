"
Preview to see the effect of a setting on the behavior of the pretty printer.

self new openWithSpec 
"
Class {
	#name : #BISettingPreviewer,
	#superclass : #ComposablePresenter,
	#instVars : [
		'settingsTree',
		'classSearchField',
		'methodSearchField',
		'formattedCheckBox',
		'sourceCodePane'
	],
	#category : #'BlueInk-Extras-Settings'
}

{ #category : #'build ui buttons' }
BISettingPreviewer class >> buildOpenBIInspectorButton [
	^ PluggableButtonMorph
		on: self
		getState: #openBIInspectorButtonState
		action: #openBIInspectorButtonAction
		label: #openBIInspectorButtonLabel
]

{ #category : #specs }
BISettingPreviewer class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn:
				[ :col | 
			col
				newRow: [ :rowUp | rowUp add: #settingsTree ];
				newRow:
						[ :rowMiddle | 
					rowMiddle
						add: #classSearchField;
						add: #methodSearchField;
						add: #formattedCheckBox ]
					height: 30;
				newRow:
						[ :rowDown | rowDown newColumn: [ :methodSourceCode | methodSourceCode add: #sourceCodePane ] ] ];
		yourself
]

{ #category : #settings }
BISettingPreviewer class >> dialogOpenBIPreviewer [
	^ Smalltalk ui theme
		newRowIn: World
		for:
			{(Smalltalk ui theme buttonLabelForText: 'Open Blue Ink Setting Previewer ').
			self buildOpenBIInspectorButton}
]

{ #category : #'instance creation' }
BISettingPreviewer class >> open [
	<script>
	self new openWithSpec 
]

{ #category : #'button behavior' }
BISettingPreviewer class >> openBIInspectorButtonAction [
	<script>
	BISettingPreviewer new openWithSpec
]

{ #category : #'button behavior' }
BISettingPreviewer class >> openBIInspectorButtonLabel [
	^ 'Open'
]

{ #category : #'button behavior' }
BISettingPreviewer class >> openBIInspectorButtonState [
	^ true
]

{ #category : #settings }
BISettingPreviewer class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #blueInkFormatterExtra)
		target: self;
		parent: #blueInkFormatter;
		label: 'BlueInk Formatting Editor';
		description: 'Open the BlueInk setting previewer to edit the formatting options';
		dialog: [ self dialogOpenBIPreviewer ]

	
]

{ #category : #'item creation' }
BISettingPreviewer >> buildClassSearchField [
	^ self newTextInput
		ghostText: 'Enter the class name...';
		autoAccept: true;
		yourself
]

{ #category : #'item creation' }
BISettingPreviewer >> buildFormattedCheckBox [
	^ self newCheckBox
		label: 'Formatted';
		state: true;
		whenActivatedDo:
				[ 
			formattedCheckBox toggleState.
			self selectorAndClassAreCorrect
				ifTrue: [ self formatSourceCode ] ];
		whenDeactivatedDo:
				[ 
			formattedCheckBox toggleState.
			self selectorAndClassAreCorrect 
				ifTrue: [ sourceCodePane text: self compiledMethodFromSearchFields sourceCode ] ];
		yourself
]

{ #category : #'item creation' }
BISettingPreviewer >> buildMethodSearchField [
	^ self newTextInput
		ghostText: 'Enter the method selector...';
		whenTextIsAccepted: [ self whenSelectorAccepted ];
		yourself
]

{ #category : #'item creation' }
BISettingPreviewer >> buildSettingsTreeModel [
	| settingsTreeModel |
	settingsTreeModel := self newTree.
	settingsTreeModel
		roots:
			((SettingTree acceptableKeywords: #(#systemsettings)) nodeList
				select: [ :node | node parentName = #blueInkFormatter ]).
	settingsTreeModel displayBlock: [ :node | self displayNodeFor: node ].
	^ settingsTreeModel
]

{ #category : #'item creation' }
BISettingPreviewer >> buildSourceCodePane [
	^ self newText
		aboutToStyle: true;
		yourself
]

{ #category : #accessing }
BISettingPreviewer >> classSearchField [
	^ classSearchField
]

{ #category : #accessing }
BISettingPreviewer >> compiledMethodFromSearchFields [
	"precondition text represent class and selector"
	
	^ (self class environment at: classSearchField text asSymbol)
			>> methodSearchField text asSymbol 
]

{ #category : #'item creation' }
BISettingPreviewer >> displayNodeFor: aNode [
	| nodeMorphLeft nodeMorphRight |
	nodeMorphLeft := StringMorph contents: aNode item label.
	nodeMorphRight := (self theme newRowIn: World for: {aNode settingDeclaration inputWidget})
		clipSubmorphs: true;
		cellInset: 0;
		width: 570;
		left: 400;
		yourself.
	^ PanelMorph new
		addAllMorphs:
			{nodeMorphLeft.
			nodeMorphRight}; 
		color: Smalltalk ui theme lightBackgroundColor;
		hResizing: #shrinkWrap;
		yourself
]

{ #category : #api }
BISettingPreviewer >> formatSourceCode [
	| source tree formatted |
	source := self compiledMethodFromSearchFields sourceCode.
	tree := RBParser parseMethod: source onError: [ :msg :pos | ^ self ].
	formatted := tree
		formattedCodeWithMaxLineLength: (self window value window value width / 9) integerPart asInteger.	"It is a bit tricky but I get the width of the sourceCodePane like this..."
	sourceCodePane text: formatted
]

{ #category : #accessing }
BISettingPreviewer >> formattedCheckBox [
	^ formattedCheckBox
]

{ #category : #api }
BISettingPreviewer >> initialExtent [
	^ 1000 @ 700
]

{ #category : #initialization }
BISettingPreviewer >> initializeWidgets [
	sourceCodePane := self buildSourceCodePane.
	classSearchField := self buildClassSearchField.
	methodSearchField := self buildMethodSearchField.
	settingsTree := self buildSettingsTreeModel.
	formattedCheckBox := self buildFormattedCheckBox.
	SystemAnnouncer uniqueInstance weak when: BISettingsChanged send: #whenASettingChanged to: self.
	self focusOrder
		add: settingsTree;
		add: classSearchField;
		add: methodSearchField;
		add: formattedCheckBox;
		add: sourceCodePane
]

{ #category : #accessing }
BISettingPreviewer >> methodSearchField [
	^ methodSearchField
]

{ #category : #testing }
BISettingPreviewer >> selectorAndClassAreCorrect [
	| class |
	methodSearchField text
		ifNotNil:
			[ :method | 
			classSearchField text
				ifNotNil:
					[ 
					class := self class environment at: classSearchField text asSymbol ifAbsent: [ ^ false].
					(class includesSelector: method asSymbol)
								ifTrue: [ ^ true ]]].
	^ false 
]

{ #category : #accessing }
BISettingPreviewer >> settingsTree [
	^ settingsTree
]

{ #category : #accessing }
BISettingPreviewer >> sourceCodePane [
	^ sourceCodePane
]

{ #category : #api }
BISettingPreviewer >> title [
	^ 'Blue Ink Setting Previewer'
]

{ #category : #'event handling' }
BISettingPreviewer >> whenASettingChanged [
	(self formattedCheckBox state and: [ self selectorAndClassAreCorrect ])
		ifTrue: [ self formatSourceCode ] 
]

{ #category : #'event handling' }
BISettingPreviewer >> whenSelectorAccepted [
	| compiledMethod |
	self selectorAndClassAreCorrect
		ifTrue:
			[ 
			compiledMethod := self compiledMethodFromSearchFields.
			sourceCodePane behavior: compiledMethod methodClass.
			self formattedCheckBox state
				ifTrue: [ self formatSourceCode ]
				ifFalse: [ sourceCodePane text: compiledMethod sourceCode ]]
		ifFalse: [ 
			sourceCodePane behavior: nil.
			sourceCodePane text: ' ' ]
]
