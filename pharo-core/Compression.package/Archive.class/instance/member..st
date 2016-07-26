private
member: aMemberOrName
	^(members includes: aMemberOrName)
		ifTrue: [ aMemberOrName ]
		ifFalse: [ self memberNamed: aMemberOrName ].