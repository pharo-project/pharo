"
I am a Test who faild at the first execution.

I will be use to tests some rerunFailed Tests features.
"
Class {
	#name : 'FailThenPass',
	#superclass : 'MockTest',
	#classInstVars : [
		'counter'
	],
	#category : 'TestRunner-Tests-Utilities-Test',
	#package : 'TestRunner-Tests',
	#tag : 'Utilities-Test'
}

{ #category : 'accessing' }
FailThenPass class >> counter [

	counter
		ifNil: [ counter := 0 ]
		ifNotNil: [ counter = 1 ifTrue: [ counter := 0 ] ifFalse: [ counter := 1 ]].
	^ counter
]

{ #category : 'accessing' }
FailThenPass class >> reset [
	<script>
	counter := nil.
]

{ #category : 'tests' }
FailThenPass >> testFailAtFirstRunning [

	self shouldBeIgnored ifTrue: [ ^ self ].
	self assert: self class counter equals: 1
]

{ #category : 'tests' }
FailThenPass >> testPass [

	self shouldBeIgnored ifTrue: [ ^ self ].
	self assert: true
]
