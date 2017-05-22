private
compare: anId
	^ (self 
		oid_cmp: self
		b: anId) isZero