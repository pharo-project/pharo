instance creation
from: oldArray to: newArray 
	^ self new 
		oldSelectedRowIndexes: oldArray;
		newSelectedRowIndexes: newArray;
		yourself