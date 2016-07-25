accessing
selectors
	^ Array streamContents: [ :stream |
		self selectorsDo: [ :selector | 
			stream nextPut: selector ]]