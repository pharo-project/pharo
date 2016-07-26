requirements
collectionWith1TimeSubcollection
" return a collection including 'oldSubCollection'  only one time "
	^ collectionWith1TimeSubcollection ifNil: [ collectionWith1TimeSubcollection := collectionWithSameAtEndAndBegining  , self oldSubCollection , collectionWithSameAtEndAndBegining  ].