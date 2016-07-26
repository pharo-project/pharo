This is a thread-safe implementation of a queue with wait-free operations. 
It is guaranteed that any message sent to my instance (like adding new item, or fetching item) will not block sender,
nor enter a waiting loop.

The implementation is based on atomicity of simple assignment operations, which can't be interrupted in a middle,
i.e. two assignment statements in a row, like:

x:=y.
y:=z.

can't be interrupted by interpreter and can be seen as a single atomic operation.


This implementation fits best for case, when multiple threads populating queue, but only single thread fetching items from it.

In given implementation, 
inserting new items into queue can't block the sender and insertion operation always takes constant time (if we ignore the possible GC interference here).
For reading/polling operations queue using a lock mechanism, which indicating that queue currently in the middle of extraction,
and therefore, if some thread obtained a lock upon the queue, other threads must wait till one that obtained the lock will finish its operation(s) and release the lock.
All operations which may block the sender will answer the default value(s) instead. 
