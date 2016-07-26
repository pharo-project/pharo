private
basicRelevantProcessorsProcess: aSpotterContext do: aBlock
	aSpotterContext text substrings 
		detect: [ :each | each beginsWith: '#' ]
		ifFound: [ :categoryQuery | 
			| categoryQueryPrefix |
			aSpotterContext text: (aSpotterContext text copyReplaceAll: categoryQuery with: '').
			categoryQueryPrefix := categoryQuery allButFirst asLowercase.
			self processors 
				select: [ :each | 
					self 
						processor: each 
						isRelevantForQuery: categoryQueryPrefix ]
				thenDo: aBlock ]
		ifNone: [ 
			self processors do: aBlock ]