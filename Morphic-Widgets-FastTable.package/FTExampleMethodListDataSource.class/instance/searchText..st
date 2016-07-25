accessing
searchText: aString
	aString isEmptyOrNil ifTrue: [ ^ #() ].
	
	self elements withIndexDo: [ :each :index |
		( each name beginsWith: aString)
			ifTrue: [ ^ { index } ] ].
	^ #()