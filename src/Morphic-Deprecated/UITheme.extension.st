Extension { #name : 'UITheme' }

{ #category : '*Morphic-Deprecated' }
UITheme >> denyIn: aThemedMorph text: aStringOrText title: aString [
	"Answer the result of an deny dialog (true) with the given label and title."

	^ ((DenyDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText) openModal cancelled not
]

{ #category : '*Morphic-Deprecated' }
UITheme >> newExpanderIn: aThemedMorph label: aString forAll: controls [
	"Answer an expander with the given label and controls."

	|answer|
	answer := ExpanderMorph basicNew
		basicTheme: self;
		initialize;
		font: self menuFont;
		titleText: aString;
		basicTheme: nil.
	controls do: [:m |
		answer addMorphBack: m].
	^answer
]

{ #category : '*Morphic-Deprecated' }
UITheme >> newMorphDropListIn: aThemedMorph for: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel useIndex: useIndex help: helpText [
	"Answer a morph drop list for the given model."

	^(MorphDropListMorph
			on: aModel
			list: listSel
			selected: getSel
			changeSelected: setSel
			useIndex: useIndex)
		selectionColor: self selectionColor;
		getEnabledSelector: enabledSel;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText
]

{ #category : '*Morphic-Deprecated' }
UITheme >> newOverflowRowIn: aThemedMorph forAll: aCollectionOfMorphs [
	"Answer a new overflow row morph with the given contents."

	^OverflowRowMorph new
		baseMorphs: aCollectionOfMorphs;
		yourself
]

{ #category : '*Morphic-Deprecated' }
UITheme >> newSliderIn: aThemedMorph for: aModel getValue: getSel setValue: setSel min: min max: max quantum: quantum getEnabled: enabledSel help: helpText [
	"Answer a slider. Resizing set for horizontal usage by default."

	^ (PluggableSliderMorph
		   on: aModel
		   getValue: getSel
		   setValue: setSel
		   min: min
		   max: max
		   quantum: quantum)
		  getEnabledSelector: enabledSel;
		  hResizing: #spaceFill;
		  vResizing: #shrinkWrap;
		  setBalloonText: helpText;
		  minHeight: 16 scaledByDisplayScaleFactor
]

{ #category : '*Morphic-Deprecated' }
UITheme >> newToolDockingBarIn: aThemedMorph [
	"Answer a new tool docking bar."

	^ToolDockingBarMorph new
		borderWidth: 0;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		clipSubmorphs: true
]
