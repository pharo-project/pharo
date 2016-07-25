A LargeIdentitySet is an IdentitySet for large collections.
Rather than using linear probing, it takes advantage of the fact that identityHash (on a classic, 32bit VM) 
in Pharo only has 4096 unique values, using a bucket for each of those.
It will still work if hash range changes (ie buckets are chosen mod 4096), 
but the potential gain will be lower the more diverse the hash space is.

With linear probing you risk an array looking like this:
Index:	Hash:
	1	X
	2	X
	3	X
	4	4
	5	5
	6	4
	7	4
	8	7
	9	6
	10	X

While with buckets the same dataset looks:
Index:	Hash:
	1	X
	2	X
	3	X
	4	[4, 4, 4]
	5	5
	6	6
	7	7
	8	X
	9	X
	10	X

So includes: can generally be done faster (also sped up byusing a special primitive), 
and removal of objects does not have to do extensive cleanup if object was part of a chain.