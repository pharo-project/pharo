instance creation
rowIndex: aNumber event: anEvent
	^ self new 
		selectedRowIndex: aNumber;
		event: anEvent;
		yourself