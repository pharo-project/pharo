"
A promis represents a computation.
A promise is an object that allow us to interact with the result of a computation strictly in asynchronic way.
A promise is registered and kept up to the end of the computation and application of the callbacks.
"
Class {
	#name : #TKTPromise,
	#superclass : #Object,
	#instVars : [
		'result',
		'valueSemaphore',
		'resultCallbacks',
		'exceptionCallbacks',
		'runner'
	],
	#category : #'TaskIt-Futures'
}
