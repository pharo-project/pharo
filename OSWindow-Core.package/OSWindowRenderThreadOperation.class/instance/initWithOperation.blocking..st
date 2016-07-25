initialize
initWithOperation: anOperation blocking: isBlocking
	semaphore := Semaphore new.
	operation := anOperation.
	blocking := isBlocking
	
	