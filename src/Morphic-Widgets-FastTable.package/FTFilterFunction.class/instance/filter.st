updating
filter
	pattern ifNil: [ ^ self ].
	table dataSource: (pattern ifEmpty: [ initialDataSource ] ifNotEmpty: [ initialDataSource newDataSourceMatching: (filterClass pattern: pattern) ]).
	table refresh.
	self isExplicite
		ifTrue: [ self resizeWidget ]