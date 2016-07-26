initialization
initializeWidgets
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