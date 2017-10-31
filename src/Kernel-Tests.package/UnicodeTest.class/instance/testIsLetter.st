tests-categorization
testIsLetter
	| letterCategories methodAnswer catFromTable |
	letterCategories := #(Ll Lm Lo Lt Lu) collect: [ :c | Unicode classPool at: c ].
	self aRandomSelectionOfCodePointsDo: [ :cp | 
			methodAnswer := Unicode isLetter: (Character codePoint: cp).
			catFromTable := self unicodeCategoryTableLookup: cp.
			self assert: methodAnswer equals: (letterCategories includes: catFromTable) ]