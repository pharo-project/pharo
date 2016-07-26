tests - sorting
testSortedUsingBlock
	| result tmp |
	result := self unsortedCollection sorted:  [:a :b | a>b]..
	tmp := result at: 1.
	result do:
		[:each | self assert: each<=tmp. tmp:= each. ].