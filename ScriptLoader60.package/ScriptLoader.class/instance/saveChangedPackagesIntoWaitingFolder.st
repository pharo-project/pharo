elementary steps
saveChangedPackagesIntoWaitingFolder
	self currentChangedPackages do: [:each |
		self
			saveInToReloadCachePackage: each
			with: self commentForCurrentUpdate]