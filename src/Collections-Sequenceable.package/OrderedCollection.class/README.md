I am one of the most common collection. I can grow, and elements can be added sequentially by the user.  

Description 
-------------------
I am more general than Array; my size grows on demand. I store data inside an Array and remember the first and last index. If I need, I can replace this Array by a larger one.

I am usually used to store an unknown amount of objects. When my contents size will not move, one can send me the #asArray message to get better performances, but I cannot grow anymore (add: and remove: are not supported on Array).

Public API and Key Messages
-------------------	

- #new / #withAll: aCollection / #with: anObject 	are common constructors

- #add: anObject / #at: anIndex put: anObject / #at: anIndex ifAbsentPut: anObject 	allow to add new elements to myself.

- #remove: anObject / #removeIndex: anIndex 	allow to remove an element.
	
- #do: aBlock / #collect: aBlock / #select: aBlock / #reject: aBlock 	are common iterators.

Examples
-------------------	
	
	"There is many ways to create an OrderedCollection, here are some:"
	ordCol := OrderedCollection new.
	ordCol
		add: 'one';
		add: 'two';
		addFirst: 'zero';
		addLast: 'three'.
	ordCol.		"returns: an OrderedCollection('zero' 'one' 'two' 'three')"

	"or"
	ordCol := OrderedCollection with: 'one' with: 'two' with: 'three'.
	ordCol.		"returns: an OrderedCollection('one' 'two' 'three')"

	"or from an other collection"
	ordCol := OrderedCollection withAll: #('one' 'two' 'three').
	ordCol.		"returns: an OrderedCollection('one' 'two' 'three')"

	"or"
	#('one' 'two' 'three') asOrderedCollection.

	"Some manipulations"
	ordCol := OrderedCollection ofSize: 2.
	ordCol
		at: 1 put: 'one';
		at: 2 put: 'two';
		at: 2 ifAbsentPut: 'three'.
	ordCol.		"returns: an OrderedCollection('one' 'two')"
	ordCol
		remove: 'two';
		removeIndex: 1.
	ordCol.		"returns:  an OrderedCollection()"

	"A last one"
	ordCol := OrderedCollection with: $b with: $c with: $a.
	ordCol sort: [ :first :second | first < second ].		"returns: an OrderedCollection($a $b $c)"
	ordCol collect: [ :element | element asUppercase ].		"returns:  an OrderedCollection($A $B $C)"
	ordCol select: [ :element | element >= $b ].		"returns:  an OrderedCollection($b $c)"
	ordCol do: [ :element | element inspect ].
	ordCol asArray		"returns: #($a $b $c)"
			 
Internal Representation and Key Implementation Points.
-------------------	

    Instance Variables
	array:			<Array> 		An Array where I store my elements. If I need a bigger one I can remove this one and create a new one.
	firstIndex:		<Integer> 	The index of my first element.
	lastIndex:		<Integer> 	The index of my last element.

I store my elements inside an array. This array is AT LEAST of the size of my elements. If someone adds an element and my array is not large enough, I remove it and I create a new one larger with the same elements (usually, the size double).