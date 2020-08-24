"
I am a dummy DebugRequest, used for testing
"
Class {
	#name : #OupsDummyDebugRequest,
	#superclass : #OupsDebugRequest,
	#instVars : [
		'dummyPreDebugActionExecutedCount'
	],
	#category : #'Oups-Tests-utils'
}

{ #category : #preDebugActions }
OupsDummyDebugRequest >> computePreDebugActions [

	"For test purposes. Instead of computing preDebugActions as usual, insert a dummy pre debug action"

	self preDebugActions add: [ :dbSession | 
		dummyPreDebugActionExecutedCount := self dummyPreDebugActionExecutedCount	+ 1 ]
]

{ #category : #preDebugActions }
OupsDummyDebugRequest >> dummyPreDebugActionExecutedCount [
	dummyPreDebugActionExecutedCount ifNil: [ dummyPreDebugActionExecutedCount := 0 ].
	^ dummyPreDebugActionExecutedCount
]
