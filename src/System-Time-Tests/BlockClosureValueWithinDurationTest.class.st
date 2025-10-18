"
SUnit tests for a fix on #valueWithin:
"
Class {
	#name : 'BlockClosureValueWithinDurationTest',
	#superclass : 'TestCase',
	#category : 'System-Time-Tests',
	#package : 'System-Time-Tests'
}

{ #category : 'tests' }
BlockClosureValueWithinDurationTest >> testValueWithinNonLocalReturn [
	"The real test for the fix is just as obscure as the original problem"

	| startTime |
	self valueWithinNonLocalReturn.
	startTime := Time millisecondClockValue.
	[[] repeat] valueWithin: 100 milliSeconds onTimeout:[ | deltaTime |
		"This *should* timeout after 100 msecs but the pending process from
		the previous invokation will signal timeout after 20 msecs already
		which will in turn cut this invokation short."
		deltaTime := Time millisecondClockValue - startTime.
		self deny: deltaTime < 90.
	]
]

{ #category : 'tests' }
BlockClosureValueWithinDurationTest >> testValueWithinTimingBasic [
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		[(Delay forMilliseconds: 1000) wait]
			valueWithin: 100 milliSeconds onTimeout: []
	] timeToRun.
	self assert: time < 1000 milliSeconds
]

{ #category : 'tests' }
BlockClosureValueWithinDurationTest >> testValueWithinTimingNestedInner [
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			[ (Delay forSeconds: 5) wait ]
				valueWithin: 100 milliSeconds onTimeout: []
		] valueWithin: 500 milliSeconds onTimeout: []
	] timeToRun.

	"The time should be closer to the 100ms than to the 500ms"
	self assert: time < 150 milliSeconds
]

{ #category : 'tests' }
BlockClosureValueWithinDurationTest >> testValueWithinTimingNestedOuter [
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			3 timesRepeat: [
				[(Delay forMilliseconds: 500) wait]
					valueWithin: 100 milliSeconds onTimeout: []]
		] valueWithin: 150 milliSeconds onTimeout: []
	] timeToRun.
	self assert: time > 100 milliSeconds.
	self assert: time < 5000 milliSeconds
]

{ #category : 'tests' }
BlockClosureValueWithinDurationTest >> testValueWithinTimingRepeat [
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		3 timesRepeat: [
			[(Delay forMilliseconds: 500) wait]
				valueWithin: 100 milliSeconds onTimeout: []]
	] timeToRun.
	self assert: time < 500 milliSeconds
]

{ #category : 'helpers' }
BlockClosureValueWithinDurationTest >> valueWithinNonLocalReturn [
	"Do a non-local return from a valueWithin: block"
	[^self] valueWithin: 20 milliSeconds onTimeout:[]
]
