Extension { #name : #DelayScheduler }

{ #category : #'*Kernel-Tests' }
DelayScheduler >> testCaseSupportActiveDelay [
	^activeDelay
]

{ #category : #'*Kernel-Tests' }
DelayScheduler >> testCaseSupportSetActiveDelayStartTime: time [
	activeDelayStartTime := time.
]

{ #category : #'*Kernel-Tests' }
DelayScheduler >> testCaseSupportSystemTimingSemaphore [
	^ timingSemaphore
]

{ #category : #'*Kernel-Tests' }
DelayScheduler >> testCaseSupportTimingSemaphore [
	"Tests might alter which sempahore the VM signals, so call this from test tearDown"
	^ timingSemaphore.
]
