"
A SemaphoreTest is sunit test for simple and multiEx semaphores

Instance Variables none; does not have common test fixture accross all tests (because its testing differenct sorts of semaphores (could refactor into muliple testcases if there were more test conditions.

"
Class {
	#name : #SemaphoreTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Processes'
}

{ #category : #private }
SemaphoreTest >> criticalError [
	Processor activeProcess terminate
]

{ #category : #tests }
SemaphoreTest >> testAfterCriticalWait [
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical: leaves it with signaling the associated semaphore."
	
	| s p |
	s := Semaphore new.
	p := [s critical:[]] forkAt: Processor activePriority-1.
	"wait until p entered the critical section"
	[p suspendingList == s] whileFalse:[(Delay forMilliseconds: 10) wait].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 1
]

{ #category : #tests }
SemaphoreTest >> testCritical [
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled
]

{ #category : #tests }
SemaphoreTest >> testCriticalIfError [
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError ifError:[]]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled
]

{ #category : #tests }
SemaphoreTest >> testInCriticalWait [
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."

	| s p |
	s := Semaphore new.
	p := [ s critical: [  ] ] fork.
	Processor yield.
	self assert: p suspendingList identicalTo: s.
	p terminate.
	self assert: (s instVarNamed: #excessSignals) equals: 0
]

{ #category : #tests }
SemaphoreTest >> testMutualExclusion [

	| lock steps |
	steps := OrderedCollection new.
	lock := Semaphore forMutualExclusion.
	
	[
		steps add: #startProcess1.
		lock critical: [
			steps add: #startCritical1.
			"waiting next process run"
			[steps includes: #startProcess2] whileFalse: [Processor yield].
			steps add: #endCritical1 ] ] fork.
	[
		"Waiting first process run"
		[steps includes: #startProcess1] whileFalse: [Processor yield].
		steps add: #startProcess2.
		lock critical: [ 
			steps add: #startCritical2.
			Processor yield.
			steps add: #endCritical2 ] ] fork.
	
	[ steps size = 6 ] whileFalse: [ Processor yield ].

	self assert: (steps hasEqualElements: #(startProcess1 startCritical1 startProcess2 endCritical1 startCritical2 endCritical2))
]

{ #category : #tests }
SemaphoreTest >> testReturnsAsManyWaitsAsSignalsReceived [

	| semaphore semaphoreReturnedOnce semaphoreReturnedTwice |
	semaphoreReturnedOnce := semaphoreReturnedTwice := false.
	semaphore := Semaphore new.
	
	[
		semaphore wait.
		semaphoreReturnedOnce := true.
		semaphore wait.
		semaphoreReturnedTwice := true ] fork.

	Processor yield.
	self deny: semaphoreReturnedOnce.
	
	semaphore signal.
	Processor yield.
	self assert: semaphoreReturnedOnce.
	self deny: semaphoreReturnedTwice.
	
	semaphore signal.
	Processor yield.
	self assert: semaphoreReturnedTwice.
]

{ #category : #tests }
SemaphoreTest >> testSchedulesFIFO [
	"Based only on wait time, independent of process priority"

	| semaphore waitingLongestResumed higherPriorityResumed |
	waitingLongestResumed := higherPriorityResumed := false.
	semaphore := Semaphore new.
	
	[
		semaphore wait.
		waitingLongestResumed := true ] forkAt: Processor activeProcess priority + 10.
	[
		semaphore wait.
		higherPriorityResumed := true ] forkAt: Processor activeProcess priority + 20.

	self deny: waitingLongestResumed.
	self deny: higherPriorityResumed.

	semaphore signal.
	self assert: waitingLongestResumed.
	
	semaphore signal.
	self assert: higherPriorityResumed.
]

{ #category : #tests }
SemaphoreTest >> testSimpleCommunication [
	"A process waiting on a signal will not proceed until one is sent"

	| semaphore wasBlockStarted wasBlockFinished |
	wasBlockStarted := wasBlockFinished := false.
	semaphore := Semaphore new.
	
	[
		wasBlockStarted := true.
		semaphore wait.
		wasBlockFinished := true ] fork.

	Processor yield.
	
	self assert: wasBlockStarted.
	self deny: wasBlockFinished.
	
	semaphore signal.
	Processor yield.
	
	self assert: wasBlockFinished.
]

{ #category : #tests }
SemaphoreTest >> testWaitTimeDuration [
	"Ensure that wait: aDuration behaves properly"

	"Ensure that a timed out wait: aDuration returns true from the wait"

	self assert: (Semaphore new wait: 50 milliSeconds) identicalTo: true.

	"Ensure that a signaled wait: aDuration returns false from the wait"
	self assert: (Semaphore new signal wait: 50 milliSeconds) identicalTo: false
]

{ #category : #tests }
SemaphoreTest >> testWaitTimeDurationWithCompletionAndTimeoutBlocks [
	"Ensure that wait:onCompletion:onTimeout: behaves properly"

	"Ensure that a timed out wait:onCompletion:onTimeout: returns the value of the timeout block"

	self assert: (Semaphore new wait: 50 milliSeconds onCompletion: [ #completed ] onTimeout: [ #timeout ]) identicalTo: #timeout.

	"Ensure that a signaled wait:onCompletion:onTimeout: returns the value of the completed block"
	self assert: (Semaphore new signal wait: 50 milliSeconds onCompletion: [ #completed ] onTimeout: [ #timeout ]) identicalTo: #completed
]

{ #category : #tests }
SemaphoreTest >> testWaitTimeoutMSecs [
	"Ensure that waitTimeoutMSecs behaves properly"

	"Ensure that a timed out waitTimeoutMSecs: returns true from the wait"

	self assert: (Semaphore new waitTimeoutMSecs: 50) identicalTo: true.

	"Ensure that a signaled waitTimeoutMSecs: returns false from the wait"
	self assert: (Semaphore new signal waitTimeoutMSecs: 50) identicalTo: false
]

{ #category : #tests }
SemaphoreTest >> testWaitTimeoutSecondsOnCompletionOnTimeout [
	"Ensure that waitTimeoutSeconds:onCompletion:onTimeout: behaves properly"

	"Ensure that a timed out waitTimeoutSeconds:onCompletion:onTimeout: returns the value of the timeout block"

	self assert: (Semaphore new waitTimeoutSeconds: 0.05 onCompletion: [ #completed ] onTimeout: [ #timeout ]) identicalTo: #timeout.

	"Ensure that a signaled waitTimeoutSeconds:onCompletion:onTimeout: returns the value of the completed block"
	self assert: (Semaphore new signal waitTimeoutSeconds: 0.05 onCompletion: [ #completed ] onTimeout: [ #timeout ]) identicalTo: #completed
]
