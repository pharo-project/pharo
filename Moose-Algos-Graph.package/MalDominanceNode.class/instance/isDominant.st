accessing
isDominant
	"A node is dominant if it has been flagged as the unique Source by one of its target
	 AND it is its own parent in the partition"
	^ uniqueSource and: [self find = self]