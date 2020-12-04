"
A profile for testing
"
Class {
	#name : #TKTTestProfile,
	#superclass : #TKTProfile,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #private }
TKTTestProfile class >> priority [

	^ -1
]

{ #category : #accessing }
TKTTestProfile class >> profileName [

	^ #test
]

{ #category : #private }
TKTTestProfile class >> profileValues [

	^ {(#debugging -> false).
		(#runner -> TKTCommonQueueWorkerPool createDefault).
		(#poolWorkerProcess -> TKTWorkerProcess).
		(#process -> Process).
		(#errorHandler -> TKTExceptionHandler).
		(#watchDog -> TKTWatchDog onDuty).
		(#processProvider -> TKTPharoProcessProvider new).
		(#serviceManager -> TKTServiceManager new)} asDictionary
]
