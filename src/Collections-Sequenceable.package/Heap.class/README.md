Heap implements a special data structure commonly referred to as 'heap' [ http://en.wikipedia.org/wiki/Heap_%28data_structure%29 ]

Heaps are good at handling priority queues because:
	1) greatest priority element according to the sort block will be stored in first position and thus accessed in O(1) operations
	2) worse time for inserting or removing an element is in O(log n) operations, where n is the size of the Heap
	Insertion/Removal times are more efficient than above upper bound, provided that:
		a) Elements are only removed at the beginning
		b) Elements are added with arbitrary sort order.
	3) there is no need to fully sort the Heap, which makes it more efficient than a SortedCollection

The heap can be fully sorted by sending the message #fullySort.
Worse time for fully sorting the Heap is in O(n log n) operations, but this is rarely used a feature.
Remind that the Heap does not fully sort the collection if you don't ask.
Thus don't expect #do: and other iterators to enumerate elements according to the sortBlock order.

Instance variables:
       array           <Array>         The data repository
       tally           <Integer>       The number of elements in the heap
       sortBlock       <Block|nil>     A two-argument block defining the sort order,
                                                       or nil in which case the default sort order is
                                                               [:element1 :element2| element1 <= element2]
       indexUpdateBlock        <Block|nil>
                                                       A two-argument block of the form [:data :index | ... ]
                                                       which allows an application object to keep track of its
                                                       index within the heap.  Useful for quick heap update
                                                       when object's sort value changes (for example, when an
                                                       object in a priority queue has its priority increased
                                                       by an external event, you don't want to have to search
                                                       through the whole heap to find the index before fixing
                                                      the heap).  No update occurs if nil.

The Heap can be viewed as a binary tree (every node in the tree has at most two children).
The root is stored in first slot of internal array.
The children are stored in next two slots.
The children of children in next four slots.
etc...

For a node A of index i (1 based), the two children B1 and B2 are thus stored in indices (2*i) and (2*i+1).
Of course, the children indices must be less than the tally otherwise they are considered inexistent.

The Heap does arrange to preserve the following invariant:
For any children B of a node A, A is sorted before B, in other words, (self sort: A before: B) = true
This implies that the root is always the first element according to sort order.
