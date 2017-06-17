private
lookupPackages
	^ (self packageEntries
		collect: [ :entry |
			entry packageName -> (IceSavedPackage 
				named: entry packageName
				repository: self frontend).
		])
		asDictionary
