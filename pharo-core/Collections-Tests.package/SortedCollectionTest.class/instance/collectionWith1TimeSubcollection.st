requirements
collectionWith1TimeSubcollection
" return a collection including 'oldSubCollection'  only one time "
	^ (SortedCollection new add: elementNoteIn ; yourself) , self oldSubCollection 