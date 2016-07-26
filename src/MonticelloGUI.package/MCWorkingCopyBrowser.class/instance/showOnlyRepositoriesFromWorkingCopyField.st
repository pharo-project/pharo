morphic ui
showOnlyRepositoriesFromWorkingCopyField
	^(DropListMorph
		on: self 
		list: #getShowOnlyRepositoriesFromWorkingCopySettings
		selected: #currentShowOnlyRepositoriesFromWorkingCopy
		changeSelected: #setShowOnlyRepositoriesFromWorkingCopy:)
		listPaneColor: Color transparent;
		yourself			
