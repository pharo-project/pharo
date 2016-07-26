user interface
setViewedPackageNames: aText 
	| allViewed |
	allViewed := Set new.
	aText asString substrings
		do: [:sub | (RPackageOrganizer default
						packageNamed: sub
						ifAbsent: [])
				ifNotNil: [:pkg | allViewed add: pkg]].
	self changePackageSet: allViewed.
	self changed: #getViewedPackageNames
