tests
testMultiplePresentationsValidation
	| browser |
	browser := GLMFinder new.
	browser show: [:a | a list. a text ].
	window := browser openOn: ($a to: $d).
	(browser panes first port: #selection) value: $z.
	self assert: (browser panes first port: #selection) value equals: $z