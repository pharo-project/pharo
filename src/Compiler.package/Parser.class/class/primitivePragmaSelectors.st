accessing
primitivePragmaSelectors
	"Answer the selectors of pragmas that specify VM primitives.
	 Needed for compile and decomple."

	^ (Pragma allNamed: #primitive from: self to: Parser)
		collect: [ :each | each methodSelector ]