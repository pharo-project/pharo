I am intended to be used in place of two arg sort blocks.

Usage

In the following example, an ascending SortFunction is created based on the result of the #first message send to each object.
#(#(1 2) #(2 3) #(0 0)) sorted: #first ascending.

To sort by the #last element, but descending, the following would be used:
#(#(1 2) #(2 3) #(0 0)) sorted: #last descending.

One can use blocks as well. The following sorts in descending order, the sub elements based on the sum of their values.
| sumBlock |
sumBlock := [:sequence | sequence inject: 0 into: [:sum :each | sum + each]].
#(#(1 2) #(2 3) #(0 0)) sorted: sumBlock descending.

One can even use 2 arg blocks, for those cases where the function isn't expressible with objects that respond to < and =. The only catch, is that such a function has to return not true and false, but instead a collation order, values of -1 (for before), 0 (the same) or 1 (to follow). For example:

| oddBlock |
oddBlock :=
		[:a :b |
		a odd = b odd ifTrue: [0] ifFalse: [a odd ifTrue: [-1] ifFalse: [1]]].
#(1 5 1 3 2 7 9 4 6) asSortedCollection: oddBlock descending

Using #undefinedFirst and #undefinedLast it is possible to deal with nil values, moving them first or last. For Example:

#(a nil z b) sorted: #value ascending undefinedFirst.
#(a nil z b) sorted: #value ascending undefinedLast.

Different scenarios are implemented by my different subclasses:
- SortByPropertyFunction implement property comparison by selector or block
- PluggableSortFunction represent two args collator block
- and ChainedSortFunction implements composition of collators created by comma message.

Applications can implement their own domain sort functinos defining the method:
	threeWayCompare: anObject1 with: anObject2

Instance Variables
	direction	<SmallInteger>	1 for ascending, -1 for descending