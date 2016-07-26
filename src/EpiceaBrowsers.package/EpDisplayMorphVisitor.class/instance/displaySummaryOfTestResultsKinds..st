private-test results
displaySummaryOfTestResultsKinds: resultSet 

	| kinds |

	kinds := 
		resultSet 
			classNamesAndSelectorsAndKindsCollect: [ :className :selector :kind | kind ] 
			as: Bag.

	^ kinds sortedElements flatCollect: [ :kindAndCount | 
		self 
			displayTestResultKind: kindAndCount key 
			with: kindAndCount value asString, ' ' ]