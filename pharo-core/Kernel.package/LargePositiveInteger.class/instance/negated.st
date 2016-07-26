arithmetic
negated 
	^ (self copyto: (LargeNegativeInteger new: self digitLength))
		normalize  "Need to normalize to catch SmallInteger minVal"