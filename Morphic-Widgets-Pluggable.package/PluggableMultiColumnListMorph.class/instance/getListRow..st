accessing
getListRow: row
	"return the strings that should appear in the requested row"
	getListElementSelector ifNotNil: [ ^model perform: getListElementSelector with: row ].
	^self getList at: row