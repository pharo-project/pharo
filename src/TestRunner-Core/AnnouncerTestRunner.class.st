"
I run tests while announcing their execution progress. I was meant to be a decorator for `ExecutionMode`.
"
Class {
	#name : 'AnnouncerTestRunner',
	#superclass : 'NewTestRunner',
	#instVars : [
		'job'
	],
	#category : 'TestRunner-Core-Extensions',
	#package : 'TestRunner-Core',
	#tag : 'Extensions'
}

{ #category : 'building' }
AnnouncerTestRunner >> buildJob: aJob [

	^ self 
		job: (aJob title: self suite name;
			max: self suite tests size;
			yourself); 
		job
]

{ #category : 'accessing' }
AnnouncerTestRunner >> job [

	^ job ifNil: [ self buildJob: Job new ]
]

{ #category : 'accessing' }
AnnouncerTestRunner >> job: anObject [

	job := anObject
]

{ #category : 'running' }
AnnouncerTestRunner >> runCase: aTestCase [

	| currentResult |
	self job title: 'Running test: ', aTestCase asString.
	currentResult := super runCase: aTestCase.
	self job currentValue: 1 + self job currentValue.
	^ currentResult 
]

{ #category : 'running' }
AnnouncerTestRunner >> runSuite [

	^ [ :newJob | 
		self buildJob: newJob.
		super runSuite ] asJob run
]
