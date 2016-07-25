user interface
getViewedPackageNames
	^ Text
		streamContents: [:s | self viewedPackages
				ifEmpty: [s nextPutAll: 'All packages' translated]
				ifNotEmpty: [:selections | 
					(selections asSet = self allPackagesWithSettings asSet)
						ifTrue: [s nextPutAll: 'All packages']
						ifFalse: [selections
								do: [:package | 
									s nextPutAll: package packageName.
									s nextPut: Character space]]]]