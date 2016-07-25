I am TTLCache. 
I am an LRUCache.

I record a timestamp when I store a value for a key.

Upon a cache hit, I check if the timestamp of the stored value does not exceed the allowed time to live duration - if so, the value has become stale and I will retrieve the value again.

The default timeToLive is 1 hour.

Note that eviction, making room in a full cache, still happens according to the LRU algorithm from my superclass - stale entries to not get evicted automatically.

I can remove all my stale values in O(n), see #removeStaleValues.

Implementation Notes

I extend my superclass by storing TTLAssociations (which also hold a timestamp) instead of Associations in the DoubleLinkedList, lruList, ordered from least to most recently used.

In case of a hit, there is now an additional check to see if the value has become stale (exceeded its time to live). If so, the value is computed again.

Timestamps are implemented using Integer seconds (Time totalSeconds) for performance reasons.