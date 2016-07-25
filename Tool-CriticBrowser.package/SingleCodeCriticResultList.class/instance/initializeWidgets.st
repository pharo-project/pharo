initialization
initializeWidgets
	criticsModel := self newIconList.
	sourceCodeModel := self newCode.
	toolbarModel := self instantiate: CriticToolbar.
	textInputFieldModel := self newTextInput.

	criticsModel beMultipleSelection.
	self initializeTextInputFieldModel.
	self initializeToolBar.
	self initializeActionToPerformOnTextModel.
	criticsModel displayBlock: [:selec |
		self initializeDisplayCritic: selec forRule: rule]. 
	
	criticsModel menu: [ :a :b | self menu: a shifted: b ].
	criticsModel icons: [ :item | self iconFor: item ].
	
	self focusOrder
		add: criticsModel;
		add: toolbarModel;
		add: sourceCodeModel.
