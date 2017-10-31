tests - basic
testSpeciesLooseSortBlock
	"This is a non regression test for http://bugs.squeak.org/view.php?id=6535"

	| numbers reverseOrder firstThree |
	numbers := #(1 2 3 4 5).
	reverseOrder := SortedCollection sortBlock: [:x :y | x > y].
	reverseOrder addAll: numbers.

	"The elements are inverted"
	self assert: (reverseOrder asArray = #(5 4 3 2 1)).

	"Copy the first 3 elements"
	firstThree := reverseOrder copyFrom: 1 to: 3.

	"It appears to work"
	self assert: (firstThree asArray = #(5 4 3)).

	"but we have lost the sort block"
	firstThree add: 1.

	" firstThree is now #(1 5 4 3)! "
	self assert: firstThree asArray = #(5 4 3 1) "fails"