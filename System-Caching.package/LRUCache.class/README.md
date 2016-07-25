I am LRUCache.
I am a Cache.

I am a limited cache that evicts the least recently used entries. My implementation is properly O(1).

Implementation Notes

The key/value pairs in the cache are held as Associations in a DoubleLinkedList, lruList, ordered from least to most recently used.

The keyIndex Dictionary maps from each key to the actual DoubleLink inside lruList holding the matching key/value pair.

New pairs are added at the end of the list.

In case of a hit, a pair gets promoted to the end of the list (most recently used).

In case of a full cache, the first pair of the list gets evicted (least recently used).

See #validateInvariantWith: where the relationship between the 2 datastructures is checked.