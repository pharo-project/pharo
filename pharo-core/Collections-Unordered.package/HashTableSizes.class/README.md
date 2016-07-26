HashTableSizes is a helper class, used by hashedCollections to determine sizes for hash tables.

Public protocol is all class-side:

#goodSizeAtLeast: anInteger 
  answers a "good" integer greater than or equal to the given integer.

An integer is not "good" as a hash table size if it is any of:
* Not prime
* Divides 256**k +- a, for small k and a
* Close to a power of two
* Close to dividing the hashMultiply constant

See Andres Valloud's hashing book, and Knuth TAOCP vol. 3.

This class caches a table of selected good primes within the positive SmallInteger range. When this table must be rebuilt, it uses an instance to compute the table. Primes are selected to keep the table fairly small, with approximately five entries per power of two.

The cached table is ordered, and is searched with a binary search to find the closest good size >= the requested size.