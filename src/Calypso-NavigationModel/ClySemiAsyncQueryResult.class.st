"
I am special kind of async query result which tries to emulate synchronous execution of fast queries.

Idea is to wait half second until background processing is finished. If it is enough time for query then for users I will be normal synchronous result. Otherwise they will see indication of processing. 

Implementation is based on semaphore which I am waiting for a half second before return to user. And my background process signals it at the end of execution. 
So in case of fast query this semahore will be signaled before delay expiration and I will return to the user with the normal synchronous state with ready to use built items.
Otherwise I will return to the user after half second with the indication of asynchronous processing.

To force semi async query execution you need convert given query using: 
	aQuery semiAsync
	
It returns ClyAsyncQuery instance with #asyncResult variable which points to me
"
Class {
	#name : #ClySemiAsyncQueryResult,
	#superclass : #ClyAsyncQueryResult,
	#instVars : [
		'asyncModeIsActive'
	],
	#category : #'Calypso-NavigationModel-Result'
}

{ #category : #building }
ClySemiAsyncQueryResult >> buildIsDone [
	asyncModeIsActive ifTrue: [ super buildIsDone ]
]

{ #category : #building }
ClySemiAsyncQueryResult >> runBuildProcess [
	"Idea is to look like sync query when it is fast enough.
	I wait half second maximum until background query will be done.
	If it will complete in time then users will not notice any progress indication
	and they will think that query was sync"
	| semaphore |
	"following flag is used to not trigger async build norifications
	when query was fast enough to be executed synchronously"
	asyncModeIsActive := false. 
	semaphore := Semaphore new.
	buildProcess := [self buildActualResult. semaphore signal] 
		forkAt: Processor userBackgroundPriority 
		named: 'Build result of ', buildingQuery printString.
	semaphore wait: 500 milliSeconds.
	asyncModeIsActive := buildProcess notNil
]
