I am a sequential collection where adjecent objects are linked.

Description
-------------------
I can store any kind of objects that I will wrap into a Link (See implementation points).
I act pretty much as an OrderedCollection, but my access times differs: the method #atIndex: is depending of the number of elements so is not constant. In the worse case, the whole list must be crossed.
Using the message #addFirst: / #removeLast causes the receiver to behave as a stack; using #addLast: / #removeFirst causes the receiver to behave as a queue.

I use the Link class (or his subclasses) in my implementation. 

Public API and Key Messages
-------------------

- #new / #withAll: aCollection / #with: anObject 	are common constructors

- #add: anObject / #at: anIndex put: anObject 	allow to add new elements to myself.

- #remove: anObject 	allow to remove an element.
	
- #do: aBlock / #collect: aBlock / #select: aBlock / #reject: aBlock 	are common iterators.

Example 
-------------------

 	"There is many ways to create a LinkedList, here are some:"
	linkedList := LinkedList new.
	linkedList
		add: 'one';
		add: 'two';
		addFirst: 'zero';
		addLast: 'three'.
	linkedList.		"returns: a LinkedList('zero' 'one' 'two' 'three')"

	"or"
	linkedList := LinkedList with: 'one' with: 'two' with: 'three'.
	linkedList.		"returns: a LinkedList('one' 'two' 'three')"

	"or from an other collection (here an Array)"
	linkedList := LinkedList withAll: #('one' 'two' 'three').
	linkedList.		"returns: a LinkedList('one' 'two' 'three')"

	"Some manipulations"
	linkedList := LinkedList new.
	linkedList
		add: 1;
		add: 2.
	linkedList.		"returns: a LinkedList(1 2)"
	linkedList remove: 1.
	linkedList.		"returns: a LinkedList(2)"

	"A last one"
	linkedList := LinkedList with: $b with: $c with: $a.
	linkedList sort: [ :first :second | first < second ].			"returns: a LinkedList($a $b $c)"
	linkedList collect: [ :element | element asUppercase ].	"returns:  a LinkedList($A $B $C)"
	linkedList select: [ :element | element >= $b ].			"returns:  a LinkedList($b $c)"
	linkedList do: [ :element | element inspect ].
	linkedList asArray									"returns: #($a $b $c)"
			
Internal Representation and Key Implementation Points.
-------------------

    Instance Variables
	firstLink:		 <Link> 	A link that contains the first value of the LinkedList.
	lastLink:		 <Link> 	A link that contains the last value of the LinkedList.

If you attempt to add any object into a LinkedList that is not a Link, it will automatically be wrapped by a ValueLink.