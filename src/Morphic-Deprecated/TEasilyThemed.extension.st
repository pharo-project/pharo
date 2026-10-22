Extension { #name : 'TEasilyThemed' }

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> deny: aStringOrText [
	"Open a denial dialog."

	^self deny: aStringOrText title: 'Access Denied'
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> deny: aStringOrText title: aString [
	"Open a denial dialog."

	^self theme
		denyIn: self
		text: aStringOrText
		title: aString
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newBracketSliderFor: aModel getValue: getSel setValue: setSel min: minValue max: maxValue quantum: quantum getEnabled: enabledSel help: helpText [
	"Answer a bracket slider with the given selectors."

	^self theme
		newBracketSliderIn: self
		for: aModel
		getValue: getSel
		setValue: setSel
		min: minValue
		max: maxValue
		quantum: quantum
		getEnabled: enabledSel
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newBracketSliderFor: aModel getValue: getSel setValue: setSel min: minValue max: maxValue quantum: quantum help: helpText [
	"Answer a bracket slider with the given selectors."

	^self
		newBracketSliderFor: aModel
		getValue: getSel
		setValue: setSel
		min: minValue
		max: maxValue
		quantum: quantum
		getEnabled: nil
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newExpander: aString [
	"Answer an expander with the given label."

	^self theme
		newExpanderIn: self
		label: aString
		forAll: #()
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newExpander: aString for: aControl [
	"Answer an expander with the given label and control."

	^self theme
		newExpanderIn: self
		label: aString
		forAll: {aControl}
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newExpander: aString forAll: controls [
	"Answer an expander with the given label and controls."

	^self theme
		newExpanderIn: self
		label: aString
		forAll: controls
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newFuzzyLabel: aString [
	"Answer a new fuzzy label."

	^self theme
		newFuzzyLabelIn: self
		for: nil
		label: aString
		offset: 1
		alpha: 0.5
		getEnabled: nil
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newFuzzyLabelFor: aModel label: aString getEnabled: enabledSel [
	"Answer a new fuzzy label."

	^self theme
		newFuzzyLabelIn: self
		for: aModel
		label: aString
		offset: 1
		alpha: 0.5
		getEnabled: enabledSel
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newFuzzyLabelFor: aModel label: aString offset: offset alpha: alpha getEnabled: enabledSel [
	"Answer a new fuzzy label."

	^self theme
		newFuzzyLabelIn: self
		for: aModel
		label: aString
		offset: offset
		alpha: alpha
		getEnabled: enabledSel
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newIncrementalSliderFor: aModel getValue: getSel setValue: setSel min: min max: max quantum: quantum getEnabled: enabledSel help: helpText [
	"Answer an inremental slider with the given selectors."

	^self theme
		newIncrementalSliderIn: self
		for: aModel
		getValue: getSel
		setValue: setSel
		min: min
		max: max
		quantum: quantum
		getEnabled: enabledSel
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newListFor: aModel list: listSelector selected: getSelector changeSelected: setSelector getEnabled: enabledSel help: helpText [
	"Answer a list for the given model."

	^self theme
		newListIn: self
		for: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		getEnabled: enabledSel
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newListFor: aModel list: listSelector selected: getSelector changeSelected: setSelector help: helpText [
	"Answer a list for the given model."

	^self
		newListFor: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		getEnabled: nil
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newListFor: aModel list: listSelector selected: getSelector changeSelected: setSelector icon: iconSelector getEnabled: enabledSel help: helpText [
	"Answer a list for the given model."

	^self theme
		newListIn: self
		for: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		icon: iconSelector
		getEnabled: enabledSel
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newMorphDropListFor: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel help: helpText [
	"Answer a morph drop list for the given model."

	^self
		newMorphDropListFor: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		getEnabled: enabledSel
		useIndex: true
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newMorphDropListFor: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel useIndex: useIndex help: helpText [
	"Answer a morph drop list for the given model."

	^self theme
		newMorphDropListIn: self
		for: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		getEnabled: enabledSel
		useIndex: useIndex
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newMorphDropListFor: aModel list: listSel getSelected: getSel setSelected: setSel help: helpText [
	"Answer a morph drop list for the given model."

	^self
		newMorphDropListFor: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		getEnabled: nil
		useIndex: true
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newMorphListFor: aModel list: listSelector getSelected: getSelector setSelected: setSelector getEnabled: enabledSel help: helpText [
	"Answer a morph list for the given model."

	^self theme
		newMorphListIn: self
		for: aModel
		list: listSelector
		getSelected: getSelector
		setSelected: setSelector
		getEnabled: enabledSel
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newMorphListFor: aModel list: listSelector getSelected: getSelector setSelected: setSelector help: helpText [
	"Answer a morph list for the given model."

	^self
		newMorphListFor: aModel
		list: listSelector
		getSelected: getSelector
		setSelected: setSelector
		getEnabled: nil
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newOverflowRowForAll: aCollectionOfMorphs [
	"Answer a new overflow row morph that provides a drop down for
	the given contents that are unable to fit the bounds."

	^self theme
		newOverflowRowIn: self
		forAll: aCollectionOfMorphs
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newSliderFor: aModel getValue: getSel setValue: setSel getEnabled: enabledSel help: helpText [
	"Answer a slider with the given selectors."

	^self theme
		newSliderIn: self
		for: aModel
		getValue: getSel
		setValue: setSel
		min: 0
		max: 1
		quantum: nil
		getEnabled: enabledSel
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newSliderFor: aModel getValue: getSel setValue: setSel help: helpText [
	"Answer a slider with the given selectors."

	^self theme
		newSliderIn: self
		for: aModel
		getValue: getSel
		setValue: setSel
		min: 0
		max: 1
		quantum: nil
		getEnabled: nil
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newSliderFor: aModel getValue: getSel setValue: setSel min: min max: max quantum: quantum getEnabled: enabledSel help: helpText [
	"Answer a slider with the given selectors."

	^self theme
		newSliderIn: self
		for: aModel
		getValue: getSel
		setValue: setSel
		min: min
		max: max
		quantum: quantum
		getEnabled: enabledSel
		help: helpText
]

{ #category : '*Morphic-Deprecated' }
TEasilyThemed >> newToolDockingBar [
	"Answer a tool docking bar."

	^self theme
		newToolDockingBarIn: self
]
