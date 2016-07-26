utils
differentMethodsWithSameSelectorBetween: firstClass and: secondClass
	| repeatedSelectors differentMethodsWithSameSelector |
	
	repeatedSelectors := firstClass localSelectors intersection: secondClass localSelectors.
	
	differentMethodsWithSameSelector := repeatedSelectors select: [ :selector | | m1 m2| 
										 m1 := firstClass>>selector. 
										 m2 := secondClass>>selector.
										 m1 sourceCode ~= m2 sourceCode].
									
	^differentMethodsWithSameSelector.