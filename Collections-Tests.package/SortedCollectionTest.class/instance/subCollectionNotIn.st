requirements
subCollectionNotIn
" return a collection for which at least one element is not included in 'accessCollection' "
	^ SortedCollection new add: elementNoteIn ; add: elementNoteIn ; yourself.