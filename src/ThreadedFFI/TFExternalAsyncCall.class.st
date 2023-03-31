"
I am an asynchronous call to an external function. I reference the function to call and the arguments of the call.
The function execution will happen in a separate thread, and meanwhile the current Pharo process is suspended without suspending the entire VM thread.

For this purpose, I have a semaphore that is handed out to the primitive that executes the function.
Upon the return of the primitive (i.e., when the function execution finished is scheduled), I wait at the semaphore.
When the function execution is managed and finishes, the threaded FFI plugin will signal the semaphore and the calling Pharo process will be re-scheduled to be run.
"
Class {
	#name : #TFExternalAsyncCall,
	#superclass : #Object,
	#instVars : [
		'function',
		'semaphore',
		'arguments',
		'parameterArray',
		'index'
	],
	#category : #'ThreadedFFI-Base'
}

{ #category : #'instance creation' }
TFExternalAsyncCall class >> forFunction: aTFExternalFunction [

	^ self basicNew
		function: aTFExternalFunction;
		initialize;
		yourself
]

{ #category : #operations }
TFExternalAsyncCall >> cleanUp [
	self deregisterSemaphore
]

{ #category : #'private - semaphore' }
TFExternalAsyncCall >> deregisterSemaphore [

	Smalltalk unregisterExternalObject: self semaphore
]

{ #category : #operations }
TFExternalAsyncCall >> doExecuteOn: aRunner [

	| aTaskAddress |

	aTaskAddress := aRunner
		executeFunction: function
		withArguments: parameterArray
		usingSemaphore: self semaphoreIndex.

	"I check if the semaphore is already signaled, because doing it in this way
	is thousands of times faster than just executing the wait.
	I think is a bug in the VM"

	self semaphore isSignaled
		ifFalse: [ self semaphore wait ].

	^ aRunner readReturnValueFromTask: aTaskAddress
]

{ #category : #operations }
TFExternalAsyncCall >> executeOn: aRunner [
	"Performs a callout using the asynchronous threaded FFI plugin.
	 This method schedules the execution of a function in the threaded FFI plugin and
	 then waits for its result in a semaphore.
	 When the semaphore is signaled, the result is available in an external value holder
	 from where the value is read.
	 This method contains some boilerplate code to allocate and release external
	 value holders for the arguments and the return value."

	^ [
			self prepareExecution.
			self doExecuteOn: aRunner ]
		ensure: [
			self cleanUp ]
]

{ #category : #accessing }
TFExternalAsyncCall >> function [
	^ function
]

{ #category : #accessing }
TFExternalAsyncCall >> function: anObject [
	function := anObject
]

{ #category : #initialization }
TFExternalAsyncCall >> initialize [
	semaphore := Semaphore new
]

{ #category : #accessing }
TFExternalAsyncCall >> parameters [
	^ arguments
]

{ #category : #accessing }
TFExternalAsyncCall >> parameters: anObject [
	arguments := anObject
]

{ #category : #operations }
TFExternalAsyncCall >> prepareExecution [
	self validateFunction.

	parameterArray := arguments asArray.

	self registerSemaphore
]

{ #category : #'private - semaphore' }
TFExternalAsyncCall >> registerSemaphore [

	index := Smalltalk registerExternalObject: self semaphore
]

{ #category : #accessing }
TFExternalAsyncCall >> semaphore [

	^ semaphore
]

{ #category : #accessing }
TFExternalAsyncCall >> semaphoreIndex [
	^ index
]

{ #category : #'private - validation' }
TFExternalAsyncCall >> validateFunction [

	function validate
]
