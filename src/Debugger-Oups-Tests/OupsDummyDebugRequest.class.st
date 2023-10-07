"
I am a dummy DebugRequest, used for testing
"
Class {
	#name : 'OupsDummyDebugRequest',
	#superclass : 'OupsDebugRequest',
	#instVars : [
		'dummyPreDebugActionExecutedCount'
	],
	#category : 'Debugger-Oups-Tests-Utils',
	#package : 'Debugger-Oups-Tests',
	#tag : 'Utils'
}

{ #category : 'preDebugActions' }
OupsDummyDebugRequest >> computePreDebugActions [

	"For test purposes. Instead of computing preDebugActions as usual, insert a dummy pre debug action"

	self preDebugActions add: [ :dbSession |
		dummyPreDebugActionExecutedCount := self dummyPreDebugActionExecutedCount	+ 1 ]
]

{ #category : 'preDebugActions' }
OupsDummyDebugRequest >> dummyPreDebugActionExecutedCount [
	^dummyPreDebugActionExecutedCount ifNil: [ dummyPreDebugActionExecutedCount := 0 ]
]
