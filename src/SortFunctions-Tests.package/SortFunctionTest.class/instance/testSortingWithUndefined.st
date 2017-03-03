tests - sorting
testSortingWithUndefined
	| data sorted |
	data := { 2@10. 5@5. 10@4. Point new. 3@8. 1@1. 11@11 }.
	sorted := data sorted: #x ascending undefinedFirst.
	self assert: sorted equals: { Point new. 1@1. 2@10. 3@8. 5@5. 10@4. 11@11 }.
	sorted := data sorted: #x descending undefinedLast.
	self assert: sorted equals: { 11@11. 10@4. 5@5. 3@8. 2@10. 1@1. Point new }.
	sorted := data sorted: #y ascending undefinedLast.
	self assert: sorted equals: { 1@1. 10@4. 5@5. 3@8. 2@10. 11@11. Point new }.
	sorted := data sorted: #y descending undefinedFirst.
	self assert: sorted equals: { Point new. 11@11. 2@10. 3@8. 5@5. 10@4. 1@1 }.