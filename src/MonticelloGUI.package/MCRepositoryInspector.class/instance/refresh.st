actions
refresh
	packages := repository packages.
	self changed: #packageList.
	self packageSelection: self packageSelection.
