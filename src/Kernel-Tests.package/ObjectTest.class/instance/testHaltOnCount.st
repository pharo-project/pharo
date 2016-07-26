tests - debugging
testHaltOnCount

	| anObject |
	anObject := Object new.
	thisContext method recompile. "reset state of #halOnce here"
	"Halts regardless of whether HaltOnce is false for the image"
	self shouldHaltWhen: [ anObject haltOnCount: 1 ].

	"Should halt if executed enough times"	
	self shouldHaltAfter: 2 times: [ anObject haltOnCount: 2 ].
	
	"Should not halt if not executed enough times"	
	self shouldntHaltAfter: 2 times: [ anObject haltOnCount: 3 ].
