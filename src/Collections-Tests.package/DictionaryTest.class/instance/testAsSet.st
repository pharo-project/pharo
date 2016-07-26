tests
testAsSet
	"Non regression test for http://bugs.squeak.org/view.php?id=7258"
	
	| aDictionary aSet assoc0 assoc1 |
	
	"Create a dictionary"
	aDictionary := Dictionary new.
	
	"Convert it to a Set"
	aSet := aDictionary asSet.

	"Add two associations to it"	
	assoc0 := #first -> 0.
	assoc1 := #first -> 1.
	aSet add: assoc0 copy; add: assoc1.
	
	"Check if the two associations were added (that should happen if they are different)"
	self
		assert: (assoc0 copy ~= assoc1) ==> (aSet size > 1)
		description:
  			'When adding two different elements, the set size should be greater than one'