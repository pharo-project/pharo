"
Additionally, TaskIt provides an alternative means to create services through blocks (or valuables actually) using `TKTParameterizableService`. An alternative implementation of the file watcher could be done as follows.

```smalltalk
service := TKTParameterizableService new.
service name: 'Generic watcher service'.
service onSetUpDo: [ Transcript show: 'File watcher started' ].
service onTearDownDo: [ Transcript show: 'File watcher finished' ].
service step: [
  'temp.txt' asFileReference exists
    ifFalse: [ Transcript show: 'file does not exist!' ] ].

service start.
```
"
Class {
	#name : 'TKTParameterizableService',
	#superclass : 'TKTService',
	#instVars : [
		'name',
		'task',
		'startUpTask',
		'tearDownTask'
	],
	#category : 'TaskIt-Services',
	#package : 'TaskIt',
	#tag : 'Services'
}

{ #category : 'initialization' }
TKTParameterizableService >> initialize [
	super initialize.
	self step: [ 100 milliSeconds wait ]
]

{ #category : 'accessing' }
TKTParameterizableService >> name [

	^ name
]

{ #category : 'accessing' }
TKTParameterizableService >> name: aString [
	name := aString
]

{ #category : 'parameterization' }
TKTParameterizableService >> onSetUpDo: aBlockClosure [

	startUpTask := aBlockClosure
]

{ #category : 'parameterization' }
TKTParameterizableService >> onTearDownDo: aBlockClosure [

	tearDownTask := aBlockClosure
]

{ #category : 'stepping' }
TKTParameterizableService >> setUp [

	super setUp.
	startUpTask value
]

{ #category : 'accessing' }
TKTParameterizableService >> step: aBlockClosure [
	task := aBlockClosure
]

{ #category : 'stepping' }
TKTParameterizableService >> stepService [
	task value
]

{ #category : 'stepping' }
TKTParameterizableService >> tearDown [

	super tearDown.
	tearDownTask value
]
