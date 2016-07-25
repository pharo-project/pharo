accessing
allFilters
	^ self allFilterSelectors collect: [ :selector | self perform: selector ]