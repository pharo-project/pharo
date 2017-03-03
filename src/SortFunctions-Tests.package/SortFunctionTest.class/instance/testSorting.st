tests - sorting
testSorting
	| data sorted |
	data := { 2@10. 5@5. 10@4. 3@8. 1@1. 11@11 }.
	sorted := data sorted: #x ascending.
	self assert: sorted equals: { 1@1. 2@10. 3@8. 5@5. 10@4. 11@11 }.
	self assert: (sorted isSortedBy: #x ascending).
	sorted := data sorted: #x descending.
	self assert: sorted equals: { 11@11. 10@4. 5@5. 3@8. 2@10. 1@1 }.
	self assert: (sorted isSortedBy: #x descending).
	sorted := data sorted: [ :each | each y printStringLength: 2 ] ascending.
	self assert: sorted equals: { 1@1. 10@4. 5@5. 3@8. 2@10. 11@11 }.
	sorted := data sorted: [ :each | each y * 100.0 ] descending.
	self assert: sorted equals: { 11@11. 2@10. 3@8. 5@5. 10@4. 1@1 }.