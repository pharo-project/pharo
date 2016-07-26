private
indexForItemName: aString andQuery: aQueryString
	^ aString 
		findString: aQueryString
		startingAt: 1 
		caseSensitive: self caseSensitive