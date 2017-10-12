accessing
numberOfColumns: aNumber
	columns := ((1 to: aNumber) collect: [ :index | FTArrayIndexColumn index: index ] )