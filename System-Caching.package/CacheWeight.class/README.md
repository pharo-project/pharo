I  am CacheWeight.
I keep track of the weight of a cache.

The weight of a cache is the sum of the weight of all values currently present. The simplest and default weight calculation returns a constant 1 for each value, effectively counting the number of values.

The default maximum is 16.

Using compute, a selector or block, applied to a value, different calculation can be made. Consider for example #sizeInMemory.