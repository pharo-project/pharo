tests - sorting
testSortUsingSortBlock
	| result tmp |
	result := self unsortedCollection sort: [:a :b | a>b].
	tmp := result at: 1.
	result do:
		[:each | self assert: each<=tmp. tmp:= each. ].