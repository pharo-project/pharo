initialization
initializeTable: tableMorph 
	self initialize.
	self 
		cellInset: tableMorph intercellSpacing";
		layoutInset: tableMorph intercellSpacing".
	self flag: #todo. "Check if layout inset works better"