"
SUnit tests for a fix on #valueWithin:
"
Class {
	#name : 'ValueWithinFixTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Extended-Processes',
	#package : 'Kernel-Tests-Extended',
	#tag : 'Processes'
}

{ #category : 'tests - to be packaged with duration package' }
ValueWithinFixTest >> testValueWithinNonLocalReturnFixReal [
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
ValueWithinFixTest >> testValueWithinNonLocalReturnFixRealMilliseconds [
	"The real test for the fix is just as obscure as the original problem"

	| startTime |
	self valueWithinNonLocalReturn.
	startTime := [] microsecondClockValue.
	[[] repeat] valueWithinMilliseconds: 100 onTimeout:[ | deltaTime |
		"This *should* timeout after 100 msecs but the pending process from
		the previous invokation will signal timeout after 20 msecs already
		which will in turn cut this invokation short."
		deltaTime := [] microsecondClockValue - startTime.
		self deny: deltaTime < 90.
	]
]

{ #category : 'tests - to be packaged with duration package' }
ValueWithinFixTest >> testValueWithinNonLocalReturnFixSimply [
	"The simple version to test the fix"

	self valueWithinNonLocalReturn.
	(Delay forMilliseconds: 50) wait
]

{ #category : 'tests' }
ValueWithinFixTest >> testValueWithinNonLocalReturnFixSimplyMilliseconds [

	self valueWithinMillisecondsNonLocalReturn.
	(Delay forMilliseconds: 50) wait
]

{ #category : 'tests - to be packaged with duration package' }
ValueWithinFixTest >> testValueWithinTimingBasic [
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		[(Delay forMilliseconds: 1000) wait]
			valueWithin: 100 milliSeconds onTimeout: []
	] timeToRun.
	self assert: time < 1000 milliSeconds
]

{ #category : 'tests' }
ValueWithinFixTest >> testValueWithinTimingBasicMilliseconds [
	
	| milli |
	milli := [
		[(Delay forMilliseconds: 1000) wait]
			valueWithinMilliseconds: 100 onTimeout: []
	] millisecondsToRun.
	self assert: milli < 1000
]

{ #category : 'tests - to be packaged with duration package' }
ValueWithinFixTest >> testValueWithinTimingNestedInner [
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			[ (Delay forSeconds: 5) wait ]
				valueWithin: 100 milliSeconds onTimeout: []
		] valueWithin: 500 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 500 milliSeconds
]

{ #category : 'tests' }
ValueWithinFixTest >> testValueWithinTimingNestedInnerMilliseconds [
	
	| milli |
	milli := [
		[
			[ (Delay forSeconds: 5) wait ]
				valueWithinMilliseconds: 100 onTimeout: []
		] valueWithinMilliseconds: 500 onTimeout: []
	] millisecondsToRun.
	self assert: milli < 500
]

{ #category : 'tests - to be packaged with duration package' }
ValueWithinFixTest >> testValueWithinTimingNestedOuter [
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			3 timesRepeat: [
				[(Delay forMilliseconds: 500) wait]
					valueWithin: 100 milliSeconds onTimeout: []]
		] valueWithin: 150 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time > 100 milliSeconds.
	self assert: time < 5000 milliSeconds
]

{ #category : 'tests' }
ValueWithinFixTest >> testValueWithinTimingNestedOutertestValueWithinTimingBasicMilliseconds [
	
	| milli |
	milli := [
		[
			3 timesRepeat: [
				[(Delay forMilliseconds: 500) wait]
					valueWithinMilliseconds: 100 onTimeout: []]
		] valueWithinMilliseconds: 150  onTimeout: []
	] millisecondsToRun.
	self assert: milli > 100.
	self assert: milli < 5000
]

{ #category : 'tests - to be packaged with duration package' }
ValueWithinFixTest >> testValueWithinTimingRepeat [
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		3 timesRepeat: [
			[(Delay forMilliseconds: 500) wait]
				valueWithin: 100 milliSeconds onTimeout: []]
	] durationToRun.
	self assert: time < 500 milliSeconds
]

{ #category : 'tests' }
ValueWithinFixTest >> testValueWithinTimingRepeatMilliseconds [
	
	| milli |
	milli := [
		3 timesRepeat: [
			[(Delay forMilliseconds: 500) wait]
				valueWithinMilliseconds: 100 onTimeout: []]
	] millisecondsToRun.
	self assert: milli < 500 
]

{ #category : 'tests' }
ValueWithinFixTest >> valueWithinMillisecondsNonLocalReturn [
	"Do a non-local return from a valueWithin: block"
	[^self] valueWithinMilliseconds: 20 onTimeout:[]
]

{ #category : 'tests - to be packaged with duration package' }
ValueWithinFixTest >> valueWithinNonLocalReturn [
	"Do a non-local return from a valueWithin: block"
	[^self] valueWithin: 20 milliSeconds onTimeout:[]
]
