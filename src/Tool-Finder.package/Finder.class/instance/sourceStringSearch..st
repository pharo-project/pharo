private
sourceStringSearch: aSearchString 
	^ self 
		methodSearch: [ :method |
			method sourceCode includesSubstring: aSearchString caseSensitive: false ]