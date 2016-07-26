private
hasLiteralSuchThat: testBlock
	"Answer true if testBlock returns true for any literal in this array, even if imbedded in 	further Arrays or CompiledMethods.  This method is only intended for private use by 	CompiledMethod 	hasLiteralSuchThat:"
	| lit |
	1 to: self size do: [:index |
		(testBlock value: (lit := self at: index)) ifTrue: [^ true].
		(lit hasLiteralSuchThat: testBlock) ifTrue: [^ true]].
	^ false