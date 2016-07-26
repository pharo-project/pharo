utils
repeatedMethodsThatDoNotAccessInstanceVariablesBetween: firstClass and: secondClass
	| repeatedSelectors repeatedMethodsThatDoNotAccessInstanceVariables |
	
	repeatedSelectors := firstClass localSelectors intersection: secondClass localSelectors.
	
	repeatedMethodsThatDoNotAccessInstanceVariables := repeatedSelectors select: [ :selector | | m1 m2| 
										 m1 := firstClass>>selector. 
										 m2 := secondClass>>selector.
										((m1 sourceCode = m2 sourceCode) and: [ m1 hasInstVarRef not ]) and: [ m2 hasInstVarRef not ]].
									
	^repeatedMethodsThatDoNotAccessInstanceVariables.