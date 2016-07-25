*GT-SpotterExtensions-Core
spotterForFilesFor: aStep
	<spotterOrder: 40>
	self flag: #specialFilter.
	aStep listProcessor
		title: 'Files';
		itemIcon: [ GLMUIThemeExtraIcons glamorousBrowse ];
		itemFilterName: [ :reference | reference basename ];
		filter: GTFilterFileReference item: [ :filter :context | 
			self fileReferencesBy: #files inContext: context usingFilter: filter ];
		wantsToDisplayOnEmptyQuery: false