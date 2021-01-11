Extension { #name : #Semaphore }

{ #category : #'*ThreadedFFI' }
Semaphore >> signalAll [
 
	[ self isEmpty ] 
	whileFalse: [ self signal ]
]
