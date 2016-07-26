deflating
insertStringAt: here
	"Insert the string at the given start position into the hash table.
	Note: The hash value is updated starting at MinMatch-1 since
	all strings before have already been inserted into the hash table
	(and the hash value is updated as well)."
	| prevEntry |
	hashValue := self updateHashAt: (here + MinMatch).
	prevEntry := hashHead at: hashValue+1.
	hashHead at: hashValue+1 put: here.
	hashTail at: (here bitAnd: WindowMask)+1 put: prevEntry.