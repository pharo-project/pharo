private
packagesNotToSavePatternNames
	^ { self theScriptLoaderPackageName, '.*'.
		'SLICE.*'.
		'Slice.*'.
		'slice.*'.
		}