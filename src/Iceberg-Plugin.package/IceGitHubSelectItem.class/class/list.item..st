instance creation
list: selectedItems item: anObject
	^ self basicNew 
		initializeList: selectedItems item: anObject;
		yourself