I am a collection close to an OrderedCollection at the exception that I keep my elements sorted by using a Block.

Description 
-------------------
Like my superclass, I am a collection that can grow in size but I keep my elements sorted.
Because of that you cannot add elements at a fix index (with #at:put: or #insert:before: methods for example).

The sort block I use should return a boolean. It takes 2 elements and return true if the first parameter should be before the second, else false.

I should be used only if you really need to keep the elements sorted all the time (but there are some exceptions, see at the end). If you do not need it, use OrderedCollection (and possibly his #sort: method).

Public API and Key Messages
-------------------

- class method: #sortBlock:  aBlock 		is a contructor. 
		
- #sort: aBlock 		is a function to change the way I am sorted. I will also update the index of my elements with the new block.

Example
-------------------

	sortColl := SortedCollection sortBlock: [ :elem1 :elem2 | elem1 < elem2 ].
	sortColl
		add: 4;
		add: 2;
		add: 1;
		add: 2.
	sortColl. 		"result: a SortedCollection(1 2 2 4)"
	
	"But you can also add a collection."
	
	sortColl addAll: #(3 5 0 6).
	sortColl. 		"result: a SortedCollection(0 1 2 2 3 4 5 6)"
			
	"You can charge the block, imagine you have a collection with number and you want at the beginning the odd elements sorted by value then the even elements sorted by value."
	
	sortColl 
		sort:
			[ :int1 :int2 | 
				((int1 even and: [ int2 even ]) or: [ int1 odd and: [ int2 odd ] ])
					ifTrue: [ int1 < int2 ]
					ifFalse: [ int1 odd ] 
			].
	sortColl 		"result: a SortedCollection(1 3 5 0 2 2 4 6)"
 
Internal Representation and Key Implementation Points.
-------------------

    Instance Variables
	sortBlock:		<Block> 		This is  a sort block used to keep me sorted. I can take 2 parameters that are two values and I return true if the first parameter should be before the second.

I refuse the methods that add elements at a fix index.

When the user is adding an element, I use some methods like #reSort or #indexForInserting: to add an element at the right position.

Discussion
----------------
(1) sort: and sortBlock: can be used to set an order to my elements but uses different implementation of the sort algorithm... See  https://pharo.fogbugz.com/f/cases/17925/Why-SortedCollection-sort-and-sortBlock-do-not-uses-the-same-method-to-sort.

(2) DO NOT USE ADDLAST:!!!! 
https://pharo.fogbugz.com/f/cases/14812/addLast-should-not-work-in-SortedCollection

x := SortedCollection with: 4 with: 3 with: 2 with: 1 with: 7.
y:=x addLast: 6; yourself.
y isSorted "-> false"