accessing
numberOfColumns: aNumber withWidths: widths
	columns := ((1 to: aNumber) collect: [ :index | 
		FTArrayIndexColumn index: index width: (widths at: index) ] )