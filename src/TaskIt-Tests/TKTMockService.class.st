"
I am a mock service used for testing purposes.
"
Class {
	#name : #TKTMockService,
	#superclass : #TKTService,
	#instVars : [
		'isStarted',
		'isShutDown',
		'hasRun'
	],
	#category : #'TaskIt-Tests-Services'
}

{ #category : #accessing }
TKTMockService >> hasRun [

	^ hasRun
]

{ #category : #initialization }
TKTMockService >> initialize [

	super initialize.
	isStarted := false.
	isShutDown := false.
	hasRun := false
]

{ #category : #accessing }
TKTMockService >> isShutDown [

	^ isShutDown
]

{ #category : #accessing }
TKTMockService >> isStarted [

	^ isStarted
]

{ #category : #accessing }
TKTMockService >> name [

	^ 'Mock service'
]

{ #category : #stepping }
TKTMockService >> setUp [

	isStarted := true
]

{ #category : #stepping }
TKTMockService >> stepService [

	hasRun := true
]

{ #category : #stepping }
TKTMockService >> tearDown [

	isShutDown := true
]
