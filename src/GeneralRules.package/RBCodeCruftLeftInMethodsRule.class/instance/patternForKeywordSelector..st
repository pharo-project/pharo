private
patternForKeywordSelector: selector
	|  index |
	^ String streamContents: [ :str |
		str nextPutAll: '`@object '.
		index := 1.
		selector keywords
			do: [ :keyword | 
				str
					nextPutAll: keyword;
					nextPutAll: ' `@arg';
					print: index ]
			separatedBy: [ 
				index := index + 1.
				str space ] ]