accessing
selectedTab
	"Answer the currently selected tab."
	
	^(self tabSelector ifNil: [^nil]) selectedTab