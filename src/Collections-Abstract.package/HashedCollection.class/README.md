I am an abstract collection of objects that implement hash and equality in a consitent way. This means that whenever two objects are equal, their hashes have to be equal too. If two objects are equal then I can only store one of them. Hashes are expected to be integers (preferably SmallIntegers). I also expect that the objects contained by me do not change their hashes. If that happens, hash invariants have to be re-established, which can be done by #rehash.

Since I'm abstract, no instances of me should exist. My subclasses should implement #scanFor:, #fixCollisionsFrom: and #noCheckNoGrowFillFrom:.

Instance Variables
	array:		<ArrayedCollection> (typically Array or WeakArray)
	tally:		<Integer> (non-negative)

array
	- An array whose size is a prime number, it's non-nil elements are the elements of the collection, and whose nil elements are empty slots. There is always at least one nil. In fact I try to keep my "load" at 75% or less so that hashing will work well.

tally
	- The number of elements in the collection. The array size is always greater than this.

Implementation details:
I implement a hash table which uses open addressing with linear probing as the method of collision resolution. Searching for an element or a free slot for an element is done by #scanFor: which should return the index of the slot in array corresponding to it's argument. When an element is removed #fixCollisionsFrom: should rehash all elements in array between the original index of the removed element, wrapping around after the last slot until reaching an empty slot. My maximum load factor (75%) is hardcoded in #atNewIndex:put:, so it can only be changed by overriding that method. When my load factor reaches this limit I replace my array with a larger one (see #grow) ensuring that my load factor will be less than or equal to 50%. The new array is filled by #noCheckNoGrowFillFrom: which should use #scanForEmptySlotFor: instead of #scanFor: for better performance. I do not shrink.
