"
I am a Test who faild at the first execution.

I will be use to tests some rerunFailed Tests features.
"
Class {
	#name : 'FailThenPassTest',
	#superclass : 'MockTest',
	#classInstVars : [
		'counter'
	],
	#category : 'TestRunner-FakeTests-Mock Tests',
	#package : 'TestRunner-FakeTests',
	#tag : 'Mock Tests'
}

{ #category : 'accessing' }
FailThenPassTest class >> counter [

	counter ifNil: [ counter := 0 ] ifNotNil: [
			counter := counter = 1
				           ifTrue: [ 0 ]
				           ifFalse: [ 1 ] ].
	^ counter
]

{ #category : 'class initialization' }
FailThenPassTest class >> reset [
	<script>
	counter := nil.
]

{ #category : 'tests' }
FailThenPassTest >> testFailAtFirstRunning [

	self assert: self class counter equals: 1
]

{ #category : 'tests' }
FailThenPassTest >> testPass [

	self assert: true
]
