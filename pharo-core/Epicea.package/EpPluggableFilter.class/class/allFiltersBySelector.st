accessing
allFiltersBySelector
	^ (self allFilterSelectors 
			collect: [ :selector | selector -> (self perform: selector) ]) 
			asDictionary