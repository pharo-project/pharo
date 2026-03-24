Extension { #name : 'SHCustomStyleElement' }

{ #category : '*STON-Extensions' }
SHCustomStyleElement >> stonOn: stonWriter [

	stonWriter writeMap: {
			(#group -> group).
			(#color -> self color).
			(#emphasis -> self emphasis) } asDictionary
]
