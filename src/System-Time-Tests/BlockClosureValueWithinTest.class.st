"
SUnit tests for a fix on #valueWithin:
"
Class {
	#name : 'BlockClosureValueWithinTest',
	#superclass : 'TestCase',
	#category : 'System-Time-Tests',
	#package : 'System-Time-Tests'
}

{ #category : 'tests' }
BlockClosureValueWithinTest >> testValueWithinNonLocalReturnDoesNotTimeout [

	self deny: self valueWithinMillisecondsWithNonLocalReturn
]

{ #category : 'tests' }
BlockClosureValueWithinTest >> testValueWithinTimingBasicMilliseconds [
	
	| milli |
	milli := [
		[(Delay forMilliseconds: 1000) wait]
			valueWithinMilliseconds: 100 onTimeout: []
	] millisecondsToRun.
	self assert: milli < 1000
]

{ #category : 'tests' }
BlockClosureValueWithinTest >> testValueWithinTimingNestedInnerMilliseconds [
	
	| milli |
	milli := [
		[
			[ (Delay forSeconds: 5) wait ]
				valueWithinMilliseconds: 100 onTimeout: []
		] valueWithinMilliseconds: 500 onTimeout: []
	] millisecondsToRun.
	self assert: milli < 500
]

{ #category : 'tests' }
BlockClosureValueWithinTest >> testValueWithinTimingNestedOutertestValueWithinTimingBasicMilliseconds [
	
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

{ #category : 'tests' }
BlockClosureValueWithinTest >> testValueWithinTimingRepeatMilliseconds [
	
	| milli |
	milli := [
		3 timesRepeat: [
			[(Delay forMilliseconds: 500) wait]
				valueWithinMilliseconds: 100 onTimeout: []]
	] millisecondsToRun.
	self assert: milli < 500 
]

{ #category : 'helpers' }
BlockClosureValueWithinTest >> valueWithinMillisecondsWithNonLocalReturn [
	"Returns true if there was a timeout.
	Uses a non-local return to exit the block"

	[ ^ false ] valueWithinMilliseconds: 20 onTimeout: [ ^ true ]
]
