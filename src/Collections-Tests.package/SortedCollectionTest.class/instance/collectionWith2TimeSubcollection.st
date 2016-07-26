requirements
collectionWith2TimeSubcollection
" return a collection including 'oldSubCollection'  two or many time "
	^ (SortedCollection  new add: elementNoteIn ; yourself) , self oldSubCollection , self oldSubCollection  