actions
pull
	self command: 'pull'.
	self loadedPackages do: #loadLatest.
	self refresh
