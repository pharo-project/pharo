browse-nautilus
browseInNautilus
	self browseMethodInNautilus:
		([ self packageUnderAnalysis packageManifest class methodNamed: #manuallyResolvedDependencies ]
			on: KeyNotFound
			do: [ PackageManifest class>>#manuallyResolvedDependencies ])