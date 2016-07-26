action
filter
	| items |
	items := OrderedCollection new.
	dataSource rootsItems do: [ :item | (self matchingFilter: item) ifNotNil: [ :itemNew | items add: itemNew ] ] displayingProgress: [ :each | 'Looking inside ' , each name ].
	dataSource table selectRowIndex: 1.
	^ dataSource class
		root:
			(FTRootItem new
				children: items asArray;
				yourself)
		children: dataSource childrenBlock