private
versionMethod

	methodHolder value
		ifNotNil: [:aMethod | self model browseVersionsFrom: aMethod ]