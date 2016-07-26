tests
testAncestorMerge
	| base revA revB revC |

	base := self snapshot.
	self change: #a toReturn: 'a1'.
	revA :=  self snapshot.
	self change: #b toReturn: 'b1'.
	revB :=  self snapshot.	
	self change: #c toReturn: 'c1'.
	revC :=  self snapshot.	

	self should: [self basicMerge: revA] raise: MCNoChangesException.
	