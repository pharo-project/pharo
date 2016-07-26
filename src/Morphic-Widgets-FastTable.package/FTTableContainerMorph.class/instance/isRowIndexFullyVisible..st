testing
isRowIndexFullyVisible: rowIndex
	"Answer if a row is *fully* visible. That means row is completely visible (there is 
	 not hidden part)"
	^ (self isRowIndexVisible: rowIndex) 
		and: [ (self isRowIndexExceding: rowIndex) not ]