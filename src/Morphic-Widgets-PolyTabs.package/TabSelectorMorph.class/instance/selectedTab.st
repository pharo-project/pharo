tabs
selectedTab
	"Answer the selected tab."
	
	^self selectedIndex = 0
		ifFalse: [self tabs at: self selectedIndex ifAbsent: [nil]]