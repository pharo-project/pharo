private
preferencesVersionFolder

	^ self preferencesGeneralFolder 
		ifNotNil: [ :folder | folder / SystemVersion current dottedMajorMinor ]