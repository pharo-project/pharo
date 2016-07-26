accessing
packages

	| packages |
	packages := OrderedCollection new.
	self loadDirective packageDirectivesDo: [:directive | packages add: directive spec ].
	^packages