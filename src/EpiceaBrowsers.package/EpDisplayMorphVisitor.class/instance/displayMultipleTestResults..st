private-test results
displayMultipleTestResults: resultSet 

	| testClasses summaryOfClasses |
	testClasses := resultSet classNames.

	summaryOfClasses := 
		(testClasses size > 1
			ifTrue: [ self displayString: testClasses size asString, ' test classes' ]
			ifFalse: [ self displayBehavior: testClasses asString  ]).

	^ summaryOfClasses, (self displayString: ': '), (self displaySummaryOfTestResultsKinds: resultSet)