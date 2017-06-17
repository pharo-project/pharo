private
refresh
	self branch refresh.
	self loadedCode refresh.
	commitDictionary := Dictionary new.
	savedPackages := nil.
	self announcer announce: (IceRepositoryModified for: self)