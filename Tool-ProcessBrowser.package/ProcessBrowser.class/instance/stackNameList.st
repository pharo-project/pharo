stack list
stackNameList
	^ stackList
		ifNil: [#()]
		ifNotNil: [stackList collect: [:each | each asString]]