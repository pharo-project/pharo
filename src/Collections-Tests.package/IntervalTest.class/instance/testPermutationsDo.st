tests
testPermutationsDo

	| i oc |
	i := (1.234 to: 4.234).
	oc := OrderedCollection new.
	i permutationsDo: [:e | oc add: e].
	self assert: (oc size = i size factorial).
	^ oc