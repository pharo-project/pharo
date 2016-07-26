tabs
updateTabs
	"Update the submorphs based on visible tabs."

	|tabsAndControls selectedTab|
	tabsAndControls := self tabsAndControls.
	self removeAllMorphs.
	tabsAndControls ifEmpty: [^self].
	selectedTab := self selectedTab.
	self tabs do: [:t |
		t selected: (t == selectedTab)].
	self
		addAllMorphs: self tabsAndControls;
		adoptPaneColor