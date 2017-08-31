comparing
valuesToCompareForEquality
	^ self instVarNamesForEqualityComparison collect: [ :ivName | self instVarNamed: ivName ]