private
classNamed: aSymbol
	^ self class environment
		at: aSymbol
		ifAbsent: [ self error: aSymbol asString , ' class not found' ]