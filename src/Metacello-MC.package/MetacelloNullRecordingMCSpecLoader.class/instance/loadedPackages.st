accessing
loadedPackages

	| packages |
	packages := OrderedCollection new.
	self loadDirective packageDirectivesDo: [:directive | packages add: directive file ].
	^packages