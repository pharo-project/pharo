PluggableSets allow the redefinition of hashing and equality by clients. This is in particular useful if the clients know about specific properties of the objects stored in the set which in turn can heavily improve the performance of sets and dictionaries.

Note: As of Pharo 1.1#11284, using normal Dictionary is actually faster as the bench below shows... ;-)

Instance variables:
	hashBlock	<BlockContext>	A one argument block used for hashing the elements.
	equalBlock	<BlockContext>	A two argument block used for comparing the elements.

Example: Adding 1000 integer points in the range (0@0) to: (100@100) to a set.

	| rnd set max pt |
	set := Set new: 1000.
	rnd := Random new.
	max := 100.
	Time millisecondsToRun:[
		1 to: 1000 do:[:i|
			pt := (rnd next * max) truncated @ (rnd next * max) truncated.
			set add: pt.
		].
	].

The above is way slow since the default hashing function of points leads to an awful lot of collisions in the set. And now the same, with a somewhat different hash function:

	| rnd set max pt |
	set := PluggableSet new: 1000.
	set hashBlock:[:item| (item x bitShift: 16) + item y].
	rnd := Random new.
	max := 100.
	Time millisecondsToRun:[
		1 to: 1000 do:[:i|
			pt := (rnd next * max) truncated @ (rnd next * max) truncated.
			set add: pt.
		].
	].
