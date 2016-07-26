building actions
contextMenuActions
	^ (Pragma
		allNamed: self contextMenuPragma
		from: self class class
		to: Object class)
		flatCollect:
			[ :eachPragma | self class perform: eachPragma methodSelector with: self ]