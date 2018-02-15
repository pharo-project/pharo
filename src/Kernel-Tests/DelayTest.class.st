"
SUnit tests for class Delay
"
Class {
	#name : #DelayTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Processes'
}

{ #category : #tests }
DelayTest >> testBounds [

	self should: [ Delay forMilliseconds: -1 ] raise: Error.
	Delay forMilliseconds: SmallInteger maxVal // 2 + 1.
	Delay forMilliseconds: SmallInteger maxVal + 1.
	(Delay forMilliseconds: Float pi) wait	"Wait 3ms"
]

{ #category : #'tests - limits' }
DelayTest >> testMultiProcessWaitOnSameDelay [
	"Ensure that waiting on the same delay from multiple processes raises an error"
	| delay p1 p2 wasRun |
	delay := Delay forSeconds: 1.
	wasRun := false.
	p1 := [delay wait] forkAt: Processor activePriority+1.
	p2 := [
		self should:[delay wait] raise: Error.
		wasRun := true.
	] forkAt: Processor activePriority+1.
	p1 terminate.
	p2 terminate.
	self assert: wasRun.


]

{ #category : #'tests - limits' }
DelayTest >> testMultiSchedule [
	"Ensure that scheduling the same delay twice raises an error"
	| delay |
	delay := Delay forSeconds: 1.
	delay schedule.
	self should:[delay schedule] raise: Error.

]

{ #category : #tests }
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

{ #category : #tests }
DelayTest >> testSemaphoreTimeout [
	"When we provide our own semaphore for a Delay, it should be used"
	"See http://bugs.squeak.org/view.php?id=6834"
	
	| sem |
	sem := Semaphore new.
	[ | process |
		process := [Delay timeoutSemaphore: sem afterMSecs: 0. sem wait] newProcess.
		process priority: Processor highIOPriority.
		process resume.
		self assert: process isTerminated.
	] ensure: [sem signal]
]
