private
calculateVisibleRows
	"Answer the rows to show in list. 
	 Ensures we show the maximum amount possible"

	^ self calculateMaxVisibleRows min: self owner numberOfRows