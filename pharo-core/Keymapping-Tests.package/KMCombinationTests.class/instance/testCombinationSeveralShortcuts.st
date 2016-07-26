tests
testCombinationSeveralShortcuts
	| shortcut platforms |
	platforms := Set new.
	
	shortcut := $a command mac | $a ctrl win | $a ctrl unix | $a alt.
	shortcut combinationsDo: [ :c | platforms add: c platform ].
	self assert: (platforms includes: #MacOSX).
	self assert: (platforms includes: #Windows).
	self assert: (platforms includes: #Unix).
	self assert: (platforms includes: #all).