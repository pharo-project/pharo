"
SUnit tests for a fix on #valueWithin:
"
Class {
	#name : #TestValueWithinFix,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Processes'
}

{ #category : #tests }
TestValueWithinFix >> testValueWithinNonLocalReturnFixReal [
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
	].

]

{ #category : #tests }
TestValueWithinFix >> testValueWithinNonLocalReturnFixSimply [
	"The simple version to test the fix"

	self valueWithinNonLocalReturn.
	(Delay forMilliseconds: 50) wait
]

{ #category : #tests }
TestValueWithinFix >> testValueWithinTimingBasic [
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		[1000 milliSeconds asDelay wait]
			valueWithin: 100 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 150 milliSeconds.
]

{ #category : #tests }
TestValueWithinFix >> testValueWithinTimingNestedInner [
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			[5 seconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []
		] valueWithin: 500 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 150 milliSeconds.

]

{ #category : #tests }
TestValueWithinFix >> testValueWithinTimingNestedOuter [
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			3 timesRepeat: [
				[5 seconds asDelay wait]
					valueWithin: 100 milliSeconds onTimeout: []]
		] valueWithin: 150 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time > 100 milliSeconds.
	self assert: time < 200 milliSeconds.
	
]

{ #category : #tests }
TestValueWithinFix >> testValueWithinTimingRepeat [
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		3 timesRepeat: [
			[500 milliSeconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []]
	] durationToRun.
	self assert: time < 450 milliSeconds.

]

{ #category : #tests }
TestValueWithinFix >> valueWithinNonLocalReturn [
	"Do a non-local return from a valueWithin: block"
	[^self] valueWithin: 20 milliSeconds onTimeout:[].

]
