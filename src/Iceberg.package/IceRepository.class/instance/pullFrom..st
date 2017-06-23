actions
pullFrom: aRemote
	self backend pullFrom: aRemote.
	self refresh.
	self loadedPackages do: #loadLatest.
