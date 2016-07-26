I am Cache.
I am an abstract class.

I am a limited cache holding onto key/value pairs.

My primary interface is #at:ifAbsentPut: which takes two arguments: a key and a block. Either the key is found (cache hit) and its value is returned, or the key is not found (cache miss). If the latter case, block should compute a new value to cache. Because block takes the key as optional argument, you can specify a factory style argument as well. With an explicit factory specified, you can also use #at: to access me.

For each addition to the cache, a weight is computed by #computeWeight (a selector or block) and added to #totalWeight. When #totalWeight is no longer below #maximumWeight, the least recently used item of the cache is evicted (removed) to make room. 

The default #computeWeight returns 1 for each value, effectively counting the number of entries. The default #maximumWeight is 16.

I count hits and misses and can return my #hitRatio.

Optionally, but not by default, I can be configured so that it is safe to access me from different threads/processess during my important operations. See #beThreadSafe.