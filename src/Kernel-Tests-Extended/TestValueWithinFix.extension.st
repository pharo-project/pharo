Extension { #name : #TestValueWithinFix }

{ #category : #'*Kernel-Tests-Extended' }
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

{ #category : #'*Kernel-Tests-Extended' }
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
