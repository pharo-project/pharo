Extension { #name : #DelayTest }

{ #category : #'*Kernel-Tests-Extended' }
DelayTest >> testSemaphoreNoTimeout [
	| sem process |
	sem := Semaphore new.
	[ | delay |
		process := [ delay := Delay timeoutSemaphore: sem afterMSecs: 1000. sem wait] newProcess.
		process priority: Processor highIOPriority.
		process resume.
		delay unschedule.
		self deny: process isTerminated.
	] ensure: [sem signal].
	self assert: process isTerminated.

]

{ #category : #'*Kernel-Tests-Extended' }
DelayTest >> testSemaphoreTimeout [
	"When we provide our own semaphore for a Delay, it should be used"
	
	| sem |
	sem := Semaphore new.
	[ | process |
		process := [Delay timeoutSemaphore: sem afterMSecs: 0. sem wait] newProcess.
		process priority: Processor highIOPriority.
		process resume.
		self assert: process isTerminated.
	] ensure: [sem signal]
]
