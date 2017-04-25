tests - sorting
testSortingChainedWithUndefined
	| data sorted |
	data := { 1@10. 5@5. 10@4. 10@nil. 3@8. 1@1. 10@10 }.
	sorted := data sorted: #x ascending , #y ascending undefinedFirst.
	self assert: sorted equals: { 1@1. 1@10. 3@8. 5@5. 10@nil. 10@4. 10@10 }.
	self assert: (sorted isSortedBy: #x ascending , #y ascending undefinedFirst).
	sorted := data sorted: #x ascending , #y descending undefinedLast.
	self assert: sorted equals: { 1@10. 1@1. 3@8. 5@5. 10@10. 10@4. 10@nil }.
	self assert: (sorted isSortedBy: #x ascending , #y descending undefinedLast).
