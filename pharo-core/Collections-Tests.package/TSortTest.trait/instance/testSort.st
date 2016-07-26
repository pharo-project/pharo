tests - sorting
testSort
	| result tmp |
	result := self unsortedCollection sort.
	tmp := result at: 1.
	result do:
		[:each | self assert: each>=tmp. tmp:= each. ].