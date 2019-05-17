Extension { #name : #HaltTest }

{ #category : #'*Kernel-Tests-WithCompiler' }
HaltTest >> testHaltFromCount [
	<haltOrBreakpointForTesting>

	| anObject haltCount |
	anObject := Object new.
	thisContext method recompile. "reset state of #halOnce here"
	"Halts regardless of whether HaltOnce is false for the image"
	self shouldHaltWhen: [ anObject haltFromCount: 1 ].

	"Should halt if executed enough times"	
	self shouldHaltAfter: 2 times: [ anObject haltFromCount: 2 ].
	
	"Should still halt if executed again after the count is reached"	
	self shouldHaltAfter: 2 times: [ anObject haltFromCount: 2 ].
	
	"Should not halt if not executed enough times"	
	self shouldntHaltAfter: 2 times: [ anObject haltFromCount: 3 ].
	
	"Should continue halting after the count is reached"
	haltCount := 0.
	10 timesRepeat: [ [ anObject haltFromCount: 3 ]
				on: Halt
				do: [ haltCount := haltCount + 1 ] ].
	self assert: haltCount equals: 8

]

{ #category : #'*Kernel-Tests-WithCompiler' }
HaltTest >> testHaltOnCount [
	<haltOrBreakpointForTesting>

	| anObject haltCount |
	anObject := Object new.
	thisContext method recompile.

	"Should not halt if not executed enough times"
	self shouldntHaltAfter: 2 times: [ anObject haltOnCount: 3 ].
	self shouldHaltAfter: 3 times: [ anObject haltOnCount: 3 ].

	"Should stop halting after the count is reached"
	haltCount := 0.
	10 timesRepeat: [ [ anObject haltOnCount: 3 ]
				on: Halt
				do: [ haltCount := haltCount + 1 ] ].
	self assert: haltCount equals: 1
]
