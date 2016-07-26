private
displaySingleTestResult: aResultSet

	| selectorsAndKinds className |
	
	"UGGGHHHHHH FIX!!!"
	selectorsAndKinds := aResultSet results anyOne selectorsAndKinds. 
	className := aResultSet results anyOne testClassName.

	^ selectorsAndKinds size = 1 
		ifTrue: [ 
			(self displayBehavior: className method: selectorsAndKinds anyOne first),
			(self displayTestResultKind: selectorsAndKinds anyOne second) ]
		ifFalse: [ 
			(self displayBehavior: className),
			(self displaySummaryOfTestResultsKinds: aResultSet ) ].