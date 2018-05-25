"
Tests related to SharedPool
"
Class {
	#name : #SharedPoolTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #tests }
SharedPoolTest >> testPoolUsers [
	| result |
	result := ChronologyConstants poolusers.
	self assert: result asSet equals: {Date. DateAndTime. Duration. Month. Time. TimeZone. Week. LocalTimeZone . AbstractTimeZone } asSet.
		 
]
