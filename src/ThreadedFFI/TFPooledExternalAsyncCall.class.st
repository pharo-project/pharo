"
I am a callout who uses the external semaphore pool you can find in each runner.
Intead creating and registering a semaphore each time, I take one from the ones available in the pool.
I return the semaphore to the list as soon as I will not use it anymore.
"
Class {
	#name : #TFPooledExternalAsyncCall,
	#superclass : #TFExternalAsyncCall,
	#category : #'ThreadedFFI-Base'
}

{ #category : #'private - semaphore' }
TFPooledExternalAsyncCall >> deregisterSemaphore [
]

{ #category : #operations }
TFPooledExternalAsyncCall >> executeOn: aRunner [
	"Performs a callout using the asynchronous threaded FFI plugin.
	 This method schedules the execution of a function in the threaded FFI plugin and 
	 then waits for its result in a semaphore.
	 When the semaphore is signaled, the result is available in an external value holder
	 from where the value is read.
	 This method contains some boilerplate code to allocate and release external 
	 value holders for the arguments and the return value."
	
	^ [
			semaphore := aRunner semaphorePool takeOne.
			self prepareExecution.
			self doExecuteOn: aRunner ]
		ensure: [
			self cleanUp.
			semaphore ifNotNil: [  
				semaphore reset.
				aRunner semaphorePool returnOne: semaphore] ]
]

{ #category : #initialization }
TFPooledExternalAsyncCall >> initialize [

]

{ #category : #'private - semaphore' }
TFPooledExternalAsyncCall >> registerSemaphore [
]

{ #category : #accessing }
TFPooledExternalAsyncCall >> reset [

	self semaphore signalAll
]

{ #category : #accessing }
TFPooledExternalAsyncCall >> semaphore [

	^ semaphore semaphore
]

{ #category : #accessing }
TFPooledExternalAsyncCall >> semaphoreIndex [

	^ semaphore index
]
