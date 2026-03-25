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
UITheme >> newBracketSliderIn: aThemedMorph for: aModel getValue: getSel setValue: setSel min: minValue max: maxValue quantum: quantum getEnabled: enabledSel help: helpText [
	"Answer a bracket slider."

	^(BracketSliderMorph
			on: aModel
			getValue: getSel
			setValue: setSel
			min: minValue
			max: maxValue
			quantum: quantum)
		getEnabledSelector: enabledSel;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
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
UITheme >> newFuzzyLabelIn: aThemedMorph for: aModel label: aString offset: offset alpha: alpha getEnabled: enabledSel [
	"Answer a new fuzzy label."

	^(FuzzyLabelMorph contents: aString font: self labelFont)
		offset: offset;
		alpha: alpha;
		model: aModel;
		getEnabledSelector: enabledSel
]

{ #category : '*Morphic-Deprecated' }
UITheme >> newIncrementalSliderIn: aThemedMorph for: aModel getValue: getSel setValue: setSel min: min max: max quantum: quantum getEnabled: enabledSel help: helpText [
	"Answer a slider inrement/decrement buttons."

	^(IncrementalSliderMorph
			on: aModel
			getValue: getSel
			setValue: setSel
			min: min
			max: max
			quantum: quantum)
		getEnabledSelector: enabledSel;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
]

{ #category : '*Morphic-Deprecated' }
UITheme >> newListIn: aThemedMorph for: aModel list: listSelector selected: getSelector changeSelected: setSelector icon: iconSelector getEnabled: enabledSel help: helpText [
	"Answer an icon list for the given model."

	^PluggableIconListMorph new
		selectionColor: self selectionColor;
		textColor: self listTextColor;
		font: self listFont;
		on: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		menu: nil
		keystroke: nil;
		autoDeselect: false;
		cornerStyle: aThemedMorph preferredCornerStyle;
		color: Color white;
		borderStyle: (BorderStyle inset width: 1 scaledByDisplayScaleFactor);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		getIconSelector: iconSelector;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText
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
UITheme >> newMorphListIn: aThemedMorph for: aModel list: listSelector getSelected: getSelector setSelected: setSelector getEnabled: enabledSel help: helpText [
	"Answer a morph list for the given model."

	^(PluggableMorphListMorph
			on: aModel
			list: listSelector
			selected: getSelector
			changeSelected: setSelector
			menu: nil
			keystroke: nil)
		selectionColor: self selectionColor;
		autoDeselect: false;
		cornerStyle: aThemedMorph preferredCornerStyle;
		color: Color white;
		borderStyle: (BorderStyle inset width: 1 scaledByDisplayScaleFactor);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		getEnabledSelector: enabledSel;
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

{ #category : '*Morphic-Deprecated' }
UITheme >> newTreeIn: aThemedMorph for: aModel list: listSelector selected: getSelector changeSelected: setSelector [
	"Answer a new tree morph."

	^TreeListMorph new
		selectionColor: self selectionColor;
		font: self listFont;
		on: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		menu: nil
		keystroke: nil;
		cornerStyle: aThemedMorph preferredCornerStyle;
		"color: Color white;"
		borderStyle: (BorderStyle inset width: 1 scaledByDisplayScaleFactor);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		autoDeselect: false;
		yourself
]

{ #category : '*Morphic-Deprecated' }
UITheme >> passwordEntryIn: aThemedMorph text: aStringOrText title: aString entryText: defaultEntryText [
	"Answer the result of a password entry dialog (a string or nil if cancelled)
	with the given label and title."

	^((PasswordDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: aStringOrText;
			entryText: defaultEntryText) openModal entryText
]
