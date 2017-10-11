helper
checkCorrespondanceOf: aSelector and: aCategoryTag 
	| cat methodAnswer catFromTable |
	cat := Unicode classPool at: aCategoryTag.
	self aRandomSelectionOfCodePointsDo: [ :cp |
		methodAnswer := Unicode perform: aSelector with: (Character codePoint: cp).
		catFromTable := Unicode generalCategory at: cp + 1.
		self assert: methodAnswer equals: (catFromTable = cat)
	]