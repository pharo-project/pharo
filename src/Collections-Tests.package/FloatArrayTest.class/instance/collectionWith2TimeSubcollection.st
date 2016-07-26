requirements
collectionWith2TimeSubcollection
" return a collection including 'oldSubCollection'  two or many time "
	^ collectionWith2TimeSubcollection ifNil: [ collectionWith2TimeSubcollection := self collectionWith1TimeSubcollection, self oldSubCollection  ].