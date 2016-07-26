*GT-SpotterExtensions-Core
spotterForDirectoriesFor: aStep
	<spotterOrder: 50>
	self flag: #specialFilter.
	aStep listProcessor
		title: 'Directories';
		itemIcon: [ GLMUIThemeExtraIcons glamorousFolder ];
		itemFilterName: [ :reference | reference basename ];
		filter: GTFilterFileReference item: [ :filter :context | 
			self fileReferencesBy: #directories inContext: context usingFilter: filter ];
		wantsToDisplayOnEmptyQuery: false	