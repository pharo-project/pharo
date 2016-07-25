enquiries
allSelectors
	| selectors |
	selectors := self subject allSelectors.
	self exclusions do: [:each |
		selectors remove: each ifAbsent: []].
	^selectors