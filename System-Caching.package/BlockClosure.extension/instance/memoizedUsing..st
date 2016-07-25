*System-Caching
memoizedUsing: cache
	"Provide a memoization block using the given cache. So one can tune by
     passing a LRUCache for instance"
  
    ^[ :x | cache at: x ifAbsentPut: [ self value: x ] ]
