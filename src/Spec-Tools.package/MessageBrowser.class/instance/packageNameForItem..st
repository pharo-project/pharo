private
packageNameForItem: anItem
	^ anItem package ifNil: [ '' ] ifNotNil: [ :package | package name ]