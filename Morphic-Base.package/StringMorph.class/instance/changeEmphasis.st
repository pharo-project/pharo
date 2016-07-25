menu
changeEmphasis

	| reply |
	reply := UIManager default 
				chooseFrom: (self emphasisChoices collect: [:t | t translated]) 
				values: self emphasisChoices.
	reply ifNotNil: [ self emphasis: (TextEmphasis perform: reply) emphasisCode ].
