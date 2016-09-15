results
updateLiterals: code
	"Add back pointer from compiled block to outer code"
	code literals do: [ :each |
		(each isKindOf: CompiledBlock) ifTrue: 
			[ each outerCode: code. ] ] 