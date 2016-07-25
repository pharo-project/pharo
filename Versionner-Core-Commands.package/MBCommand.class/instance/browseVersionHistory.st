versions
browseVersionHistory
	"Used to browse version history of a Configuration package or a package"
	
	target workingCopy ancestors isEmpty
		ifTrue: [ ^ self ].
	(MCVersionHistoryBrowser new ancestry: target workingCopy ancestors first) perform: #show