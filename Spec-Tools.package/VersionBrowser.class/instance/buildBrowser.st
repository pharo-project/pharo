instance creation
buildBrowser
	| changeList b |

	changeList := self buildChangeList.
	
	self browserForList: changeList.
	
	b := browser
		messages: changeList;
		title: 'Recent versions of ' , rgMethod asString;
		openWithSpec.
	b takeKeyboardFocus.
		
	self showDiffMorphWithConverter: (DiffMorphChangeRecordConverter 
		methodReference: browser textConverter method 
		referencesList: changeList).
		
	^ b	