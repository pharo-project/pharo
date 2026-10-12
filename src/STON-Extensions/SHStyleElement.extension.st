Extension { #name : 'SHStyleElement' }

{ #category : '*STON-Extensions' }
SHStyleElement >> stonOn: stonWriter [

	stonWriter writeMap: {
			('group' -> group).
			('color' -> self color).
			('emphasis' -> self emphasis) } asDictionary
]
