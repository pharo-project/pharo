moving changes
removeClassChanges: class
	"Remove all memory of changes associated with this class"

	| cname |
	cname := class isString
		ifTrue: [ class ]
		ifFalse: [ class name ].
	changeRecords removeKey: cname ifAbsent: [  ]