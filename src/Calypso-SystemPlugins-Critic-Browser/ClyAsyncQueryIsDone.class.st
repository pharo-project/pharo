"
I am announcing the async query completion.
Every async query announces me at the end of execution when result is completelly built.

To create instance use: 

	ClyAsyncQueryIsDone with: anAsyncQueryResult
 
Internal Representation and Key Implementation Points.

    Instance Variables
	queryResult:		<ClyAsyncQueryResult>
"
Class {
	#name : 'ClyAsyncQueryIsDone',
	#superclass : 'Announcement',
	#instVars : [
		'queryResult'
	],
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'instance creation' }
ClyAsyncQueryIsDone class >> with: anAsyncQueryResult [
	^self new
		queryResult: anAsyncQueryResult
]

{ #category : 'critics' }
ClyAsyncQueryIsDone >> affectsCritiques [
	^false
]

{ #category : 'accessing' }
ClyAsyncQueryIsDone >> queryResult [
	^ queryResult
]

{ #category : 'accessing' }
ClyAsyncQueryIsDone >> queryResult: anObject [
	queryResult := anObject
]
