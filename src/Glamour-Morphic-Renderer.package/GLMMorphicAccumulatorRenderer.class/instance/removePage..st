private
removePage: aPage
	| removedPageIndex |
	removedPageIndex := self pages indexOf: aPage.
	self announcer suspendAllWhile: [ super removePage: aPage ].
	self announcer announce: 
		(LazyTabPageRemoved new 
			tabs: self; 
			page: aPage; 
			pageIndex: removedPageIndex;
			newIndex: self tabSelectorMorph selectedIndex).
	self pages isEmpty 
		ifTrue: [ self contentMorph removeAllMorphs ]