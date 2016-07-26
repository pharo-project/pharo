assignments
refineTypesByLookingAtAssignments
	| searcher needsSearch |
	needsSearch := false.
	searcher := RBParseTreeSearcher new.
	variableTypes keysAndValuesDo: 
			[:key :value | 
			(key first = $-) 
				ifFalse: 
					[needsSearch := true.
					searcher matches: key , ' := ``@object'
						do: [:aNode :answer | self guessTypeFromAssignment: aNode]]].
	needsSearch ifTrue: [self executeSearch: searcher]