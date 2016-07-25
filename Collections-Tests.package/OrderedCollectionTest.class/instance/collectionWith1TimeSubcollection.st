requirements
collectionWith1TimeSubcollection
" return a collection including 'oldSubCollection'  only one time "
	^ ((OrderedCollection new add: elementNotIn; yourself),self oldSubCollection) add: elementNotIn;yourself   