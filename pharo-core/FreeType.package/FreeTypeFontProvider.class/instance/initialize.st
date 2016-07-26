initialization
initialize
	super initialize.
	fileInfos := OrderedCollection  new: 100.
	fileInfoCache := Dictionary new: 100. "keyed by file size" 
	families := Dictionary new.
	fontInstallers := OrderedCollection new.
	