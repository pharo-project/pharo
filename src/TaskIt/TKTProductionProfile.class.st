"
The production profile. 
Deployed applications should use this.
"
Class {
	#name : #TKTProductionProfile,
	#superclass : #TKTProfile,
	#category : #'TaskIt-Profiles'
}

{ #category : #private }
TKTProductionProfile class >> priority [

	^ 0
]

{ #category : #accessing }
TKTProductionProfile class >> profileName [
	
	^ #production
]

{ #category : #private }
TKTProductionProfile class >> profileValues [

	^ {(#debugging -> false) .
		(#runner -> TKTCommonQueueWorkerPool createDefault).
		(#poolWorkerProcess -> TKTWorkerProcess).
		(#process -> Process).
		(#watchDog -> TKTWatchDog onDuty) .
		(#errorHandler -> TKTExceptionHandler).
		(#processProvider -> TKTPharoProcessProvider new).
		(#serviceManager -> TKTServiceManager new)} asDictionary
]
