accessing
getChildren
	^ dataSource childrenBlock isSymbol
		ifTrue: [ dataSource childrenBlock value: data ]
		ifFalse: [ dataSource childrenBlock cull: data cull: self ]