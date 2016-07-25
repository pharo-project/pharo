public
unloadPackage: aPackage
	self new
		unloadPackage: aPackage;
		loadWithNameLike: aPackage name, '-unload'