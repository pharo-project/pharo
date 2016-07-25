running
debugAsFailure
	| semaphore |
	semaphore := Semaphore new.
	[ 
	semaphore wait.
	self classForTestResource resetResources: self resources ] fork.
	(self class selector: testSelector) runCaseAsFailure: semaphore