requirements
collectionWith2TimeSubcollection
" return a collection including 'oldSubCollection'  two or many time "
	^ (((OrderedCollection new add: elementNotIn; yourself),self oldSubCollection ) add: elementNotIn;yourself),self  oldSubCollection