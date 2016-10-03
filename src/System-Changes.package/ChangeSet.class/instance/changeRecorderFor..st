private
changeRecorderFor: class
	| cname |
	cname := class isString
		ifTrue: [ class ]
		ifFalse: [ class isBehavior ifTrue: [class name] ifFalse: [ nil] ].
	^ changeRecords at: cname ifAbsent: [ ^ changeRecords at: cname put: (ClassChangeRecord new initFor: cname) ]