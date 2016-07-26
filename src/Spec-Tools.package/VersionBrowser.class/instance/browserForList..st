instance creation
browserForList: aList

	browser :=  MessageBrowser new.
	browser
		topologicSort: false;
		refreshingBlock: [:m | false];
		sortingBlock: [:a :b | (a annotationNamed:#versionIndex ifAbsent:[0]) < (b annotationNamed:#versionIndex ifAbsent:[0]) ];
		displayBlock: [:changeRecord | self displayStringsFor: changeRecord].
										
	browser toolbarModel
		emptyDropList;
		addItemLabeled: 'Side By Side' do: [ 
			self showDiffMorphWithConverter: (DiffMorphChangeRecordConverter 
				methodReference: browser textConverter method 
				referencesList: aList) ];
		addItemLabeled: 'Source' do: [ 
			self showSourceWithConverter: (SourceMethodConverter method: browser textConverter method) ];
		addItemLabeled: 'Diff' do: [ 
			self showSourceWithConverter: (DiffChangeRecordConverter 
				methodReference: browser textConverter method 
				referencesList: aList) ].
		
	browser toolbarModel versionModel 
		label: 'Revert';
		action: [ self revert: browser selectedItem ].
	browser listModel menu:[:menu | menu addAllFromPragma:'versionBrowserListMenu' target:self].
	^ browser.