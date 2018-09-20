"
Tests related to SharedPool
"
Class {
	#name : #SharedPoolTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #tests }
SharedPoolTest >> testIncludesKey [

	self assert: (ChronologyConstants includesKey: #DayNames).
	self deny: (ChronologyConstants includesKey: #DependentsFields)
]

{ #category : #tests }
SharedPoolTest >> testPoolUsers [
	| result |
	result := ChronologyConstants poolUsers.
	self assert: result asSet equals: {Date. DateAndTime. Duration. Month. Time. TimeZone. Week. LocalTimeZone . AbstractTimeZone } asSet.
		 
]
