morphic ui
order: anInteger
	self class order: (order := anInteger).
	self changed: #versionList.