model access
getListSize
	"return the current number of items in the displayed list"
	getListSizeSelector ifNotNil: [ ^model perform: getListSizeSelector ].
	^self getList size