private
runClient: block
	| semaphore |
	semaphore := Semaphore new.
	[ block ensure: [ semaphore signal ] ] forkAt: self serverPriority - 1.
	semaphore wait