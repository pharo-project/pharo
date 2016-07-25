accessing
createSearchWith: aCodeString selectors: selectorCollection inClass: aClass 
	| search |
	search := RBParseTreeSearcher new.
	search matches: aCodeString
		do: 
			[:aNode :answer | 
			answer
				remove: self currentSelector ifAbsent: [self error: 'failed'];
				yourself].
	search answer: selectorCollection asBag.
	(classSearches at: aClass ifAbsentPut: [Set new]) add: search