Extension { #name : #HaltTest }

{ #category : #'*Kernel-Tests-Extended' }
HaltTest >> testHaltOnce [
	<haltOrBreakpointForTesting>
	| anObject block |
	anObject := Object new.
	block :=  [ anObject haltOnce ]. 
	self should: block raise: Halt.
	"block value."
	Halt resetOnce.
]
