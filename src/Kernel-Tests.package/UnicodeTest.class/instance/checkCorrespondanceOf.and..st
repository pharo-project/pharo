helper
checkCorrespondanceOf: aSelector and: aCategoryTag 
	| cat methodAnswer catFromTable |
	cat := Unicode classPool at: aCategoryTag.
	self aRandomSelectionOfCodePointsDo: [ :cp |
		methodAnswer := Unicode perform: aSelector with: (Character codePoint: cp).
		catFromTable := self unicodeCategoryTableLookup: cp.
		(methodAnswer = (catFromTable = cat)) ifFalse: [ 
			Transcript show: aSelector; show: 'and category '; show: aCategoryTag ;
			show: 'disagree at U+'; show: cp asHexString ; cr ].
		self assert: methodAnswer equals: (catFromTable = cat)
	]