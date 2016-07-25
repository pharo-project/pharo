accessing
getListSize
	| l |
	getListSizeSelector ifNotNil: [ ^model perform: getListSizeSelector ].

	l := self getList.
	l isEmpty ifTrue: [ ^ 0 ].
	^l size