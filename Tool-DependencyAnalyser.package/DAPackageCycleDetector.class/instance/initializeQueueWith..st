initialization
initializeQueueWith: package

	package bfsParent: nil.	
	package inStack: true.
	queue := WaitfreeQueue new.
	queue nextPut: package