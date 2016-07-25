private
updateAvailableRows: aNumber
	
	
	availableRows := availableRows + aNumber.

	aNumber isZero ifFalse: [ self tableRefresh ].