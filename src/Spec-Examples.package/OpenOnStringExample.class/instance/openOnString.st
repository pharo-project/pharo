as yet unclassified
openOnString
	| ui temp object |
	object whenChangedDo: [ :o | ui label label: o asString ].
	temp := object value.
