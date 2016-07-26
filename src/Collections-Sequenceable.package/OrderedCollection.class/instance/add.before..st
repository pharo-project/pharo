adding
add: newObject before: oldObject 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just preceding oldObject. Answer newObject."
	
	| index |
	index := self find: oldObject.
	self insert: newObject before: index.
	^newObject