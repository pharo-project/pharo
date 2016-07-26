I am a collection that act as a Dictionary except that I use key insertion order when enumerating, printing, or returing collections of keys/values/associations, but not when testing for equality (but it does not matters in this case).

I will assume that you know the Dictionary class in this comment.

Description
--------------------

I work mainly as a Dictionary except that I also store the keys in an Array that keeps the order of elements. 

I should be used ONLY if you need to keep the keys ordered. Else you should use a Dictionary that is faster and keep less values into memory. (I duplicate the keys).
Insertion, update, and inclusion testing have O(1) complexity while removing has O(n) worst-case.

Public API and Key Messages
--------------------

- #at: aKey put: aValue / #at: aKey ifAbsentPut: aValue 		allow to add an element.
  
- #at: aKey / #at: aKey ifAbsent: aBlock / #at: aKey ifPresent: aBlock ifAbsent: aBlock 		allow to access my values.

- #keysDo: aBlock / #valuesDo: aBlock / #associationsDo: 		allow to iterate on me effectively.
		
- #keyAtIndex: anIndex / KeyAtIndex: anIndex ifAbsent: aBlock 		allow to acess my keys from an index.

Examples
------------------

	"For basic examples see Dictionary comment."
	
	ordDic := (Dictionary with: 1 -> $a with: 2 -> $b) asOrderedDictionary.
	ordDic.   		"returns:  an OrderedDictionary(1->$a 2->$b)"
	ordDic keyAtIndex: 2.		"returns:  2"
	
Internal Representation and Key Implementation Points.
-------------------

    Instance Variables
	dictionary:			<Dictionary>		A dictionary where I store my keys and values.
	orderedKeys:		<Array>			An ordered collection where I store my keys to maintain the order.

I base my implementation on a Dictionary and when I need to execute an action where the order of the values is important I use the keys in my ordered collection.