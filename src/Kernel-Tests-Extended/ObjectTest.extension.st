Extension { #name : #ObjectTest }

{ #category : #'*Kernel-Tests-Extended' }
ObjectTest >> testHaltOnce [
	<haltOrBreakpointForTesting>
	| anObject block |
	anObject := Object new.
	block :=  [ anObject haltOnce ]. 
	self should: block raise: Halt.
	"block value."
	Halt resetOnce.
]
